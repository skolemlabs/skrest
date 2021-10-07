open Rresult
module C = Cohttp_lwt_unix.Client

(* Module-level errors and related boilerplate *)

type error =
  | Too_many_redirects
  | Unexpected_redirect_body_content of Uri.t
  | Missing_redirect_header of Uri.t
  | Unhandled_response_code of unhandled_response_code
  | Connection_error of Unix.error * string
  | Trapped_exception of exn * Printexc.raw_backtrace
  | Timed_out

and unhandled_response_code = {
  uri : Uri.t;
  status : Cohttp.Code.status_code;
  body : string;
}

type nonrec 'a result = ('a, [ `Skrest of error ]) result

let error e = R.error @@ `Skrest e
let error_lwt e = Lwt.return @@ error e

let pp_error fmt (`Skrest err) =
  match err with
  | Too_many_redirects -> Fmt.string fmt "Too many redirects"
  | Unexpected_redirect_body_content uri ->
    Fmt.pf fmt "Unexpected redirect body content from %a" Uri.pp_hum uri
  | Missing_redirect_header uri ->
    Fmt.pf fmt "Missing redirect header from %a" Uri.pp_hum uri
  | Unhandled_response_code { uri; status; _ } ->
    Fmt.pf fmt "Unhandled response %s from %a"
      (Cohttp.Code.string_of_status status)
      Uri.pp_hum uri
  | Connection_error (_err, message) -> Fmt.pf fmt "Connection error %s" message
  | Trapped_exception (exn, _backtrace) ->
    Fmt.pf fmt "Trapped cohttp exception %s" (Printexc.to_string exn)
  | Timed_out -> Fmt.string fmt "Request timed out"

let open_error = function
  | Ok _ as o -> o
  | Error (`Skrest _) as e -> e

let error_to_msg : 'a result -> ('a, [> `Msg of string ]) Stdlib.result =
  function
  | Ok _ as o -> o
  | Error err -> Error (`Msg (Fmt.strf "%a" pp_error err))

type 'body body =
  | Consume : string body
  | Drain : unit body
  | Stream : Cohttp_lwt.Body.t body

type 'body t = {
  response : Cohttp.Response.t;
  body : 'body;
}

let of_body :
    type a. Cohttp.Response.t -> a body -> Cohttp_lwt.Body.t -> a t Lwt.t =
 fun response body ->
  match body with
  | Consume ->
    fun b ->
      let%lwt body = Cohttp_lwt.Body.to_string b in
      Lwt.return { response; body }
  | Drain ->
    fun b ->
      let%lwt () = Cohttp_lwt.Body.drain_body b in
      Lwt.return { response; body = () }
  | Stream -> fun b -> Lwt.return { response; body = b }

let default_timeout = ref 30.0

(* Can Unix_error *)
let wrap_unix_error ~timeout f x =
  let timeout =
    let duration =
      match timeout with
      | None -> !default_timeout
      | Some duration -> duration
    in
    let%lwt () = Lwt_unix.sleep duration in
    error_lwt Timed_out
  in
  try%lwt Lwt.pick [ f x; timeout ] with
  | Unix.Unix_error (err, func, arg) ->
    let message = Fmt.strf "%s from %s(%s)" (Unix.error_message err) func arg in
    error_lwt @@ Connection_error (err, message)
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    error_lwt @@ Trapped_exception (exn, bt)

let apm_name ~meth ~uri = Fmt.str "%s: %s" meth (Uri.to_string uri)

let wrap_with_transaction
    ?(apm : Skapm.Transaction.t option)
    ?(apm_tags : Skapm.Tag.t list option)
    ~name
    ~subtype
    ~action
    f
    x =
  let open Lwt in
  match apm with
  | Some t ->
    let context = Skapm.Span.Context.make ?tags:apm_tags () in
    let span =
      Skapm.Span.make_span ~context ~parent:(`Transaction t) ~name
        ~type_:"Web Request" ~subtype ~action ()
    in
    f x >|= fun res ->
    let (_ : Skapm.Span.result) =
      Skapm.Span.finalize_and_send span
    in
    res
  | None -> f x

module type Response = sig
  type response

  val make : Cohttp_lwt.Response.t -> Cohttp_lwt.Body.t -> response result Lwt.t
end

(* Return a string containing the response body *)
module String_response : Response with type response = string = struct
  type response = string

  let make response body =
    let%lwt r = of_body response Consume body in
    Lwt.return_ok r.body
end

(* Return a [string t] value, containing a consumed response body *)
module String_t_response : Response with type response = string t = struct
  type response = string t

  let make response body =
    let%lwt r = of_body response Consume body in
    Lwt.return_ok r
end

(* Return a [unit t] value, where the body has been consumed and ignored *)
module Drain_t_response : Response with type response = unit t = struct
  type response = unit t

  let make response body =
    let%lwt r = of_body response Drain body in
    Lwt.return_ok r
end

(* Return a [Cohttp_lwt.Body.t t] value, where the body has not been consumed
   yet and the connection is still alive *)
module Stream_t_response : Response with type response = Cohttp_lwt.Body.t t =
struct
  type response = Cohttp_lwt.Body.t t

  let make response body =
    let%lwt r = of_body response Stream body in
    Lwt.return_ok r
end

(* The stuff we care about *)

(* This signature defines the verbs/operations which can be performed. The
   implementation will determine how responses are handled. *)
module type S = sig
  type response

  val head :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    Uri.t ->
    Cohttp.Response.t result Lwt.t

  val get :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    follow:int ->
    Uri.t ->
    response result Lwt.t

  val delete :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    Uri.t ->
    response result Lwt.t

  val patch :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    Uri.t ->
    response result Lwt.t

  val post :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    Uri.t ->
    response result Lwt.t

  val put :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    Uri.t ->
    response result Lwt.t

  val post_form :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    params:(string * string list) list ->
    Uri.t ->
    response result Lwt.t

  val call :
    ?ctx:Cohttp_lwt_unix.Client.ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    ?apm:Skapm.Transaction.t ->
    ?apm_tags:Skapm.Tag.t list ->
    Cohttp.Code.meth ->
    Uri.t ->
    response result Lwt.t

  val retry :
    ?wait:float ->
    retries:int ->
    Uri.t ->
    (Uri.t -> response result Lwt.t) ->
    unit ->
    response result Lwt.t
end

module Make (Response : Response) : S with type response = Response.response =
struct
  type response = Response.response

  let always_close headers =
    Cohttp.Header.add_opt_unless_exists headers "connection" "close"

  let head ?ctx ?headers ?timeout ?apm ?apm_tags uri =
    let run uri =
      let headers = always_close headers in
      let%lwt result = C.head ?ctx ~headers uri in
      Lwt.return @@ Ok result
    in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:"HEAD" ~uri)
         ~subtype:"HEAD" ~action:"Skrest#head" run
      )
      uri

  let rec get ?ctx ?headers ~follow uri =
    let%lwt (response, body) = C.get ?ctx ?headers uri in
    match Cohttp.Response.(response.status) with
    | #Cohttp.Code.success_status -> Response.make response body
    | `Found
    | `Temporary_redirect ->
      if follow > 0 then
        if%lwt Cohttp_lwt.Body.is_empty body then (
          let response_headers = Cohttp.Response.headers response in
          match Cohttp.Header.get_location response_headers with
          | Some uri -> get ?ctx ?headers ~follow:(pred follow) uri
          | None -> error_lwt (Missing_redirect_header uri)
        ) else
          error_lwt (Unexpected_redirect_body_content uri)
      else
        error_lwt Too_many_redirects
    | status ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      error_lwt (Unhandled_response_code { uri; status; body })

  let get ?ctx ?headers ?timeout ?apm ?apm_tags ~follow uri =
    let headers = always_close headers in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:"GET" ~uri)
         ~subtype:"GET" ~action:"Skrest#get"
         (get ?ctx ~headers ~follow)
      )
      uri

  let delete ?ctx ?headers uri =
    let%lwt (response, body) = C.delete ?ctx ?headers uri in
    match Cohttp.Response.(response.status) with
    | #Cohttp.Code.success_status -> Response.make response body
    | status ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      error_lwt (Unhandled_response_code { uri; status; body })

  let delete ?ctx ?headers ?timeout ?apm ?apm_tags uri =
    let headers = always_close headers in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:"DELETE" ~uri)
         ~subtype:"DELETE" ~action:"Skrest#delete" (delete ?ctx ~headers)
      )
      uri

  let patch ?ctx ?headers ?body uri =
    let%lwt (response, body) = C.patch ?ctx ?headers ?body uri in
    match Cohttp.Response.(response.status) with
    | #Cohttp.Code.success_status -> Response.make response body
    | status ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      error_lwt (Unhandled_response_code { uri; status; body })

  let patch ?ctx ?headers ?timeout ?body ?apm ?apm_tags uri =
    let headers = always_close headers in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:"PATCH" ~uri)
         ~subtype:"PATCH" ~action:"Skrest#patch"
         (patch ?ctx ~headers ?body)
      )
      uri

  let post ?ctx ?headers ?body uri =
    let%lwt (response, body) = C.post ?ctx ?headers ?body uri in
    match Cohttp.Response.(response.status) with
    | #Cohttp.Code.success_status -> Response.make response body
    | status ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      error_lwt (Unhandled_response_code { uri; status; body })

  let post ?ctx ?headers ?timeout ?body ?apm ?apm_tags uri =
    let headers = always_close headers in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:"GET" ~uri)
         ~subtype:"GET" ~action:"Skrest#post" (post ?ctx ~headers ?body)
      )
      uri

  let put ?ctx ?headers ?body uri =
    let%lwt (response, body) = C.put ?ctx ?headers ?body uri in
    match Cohttp.Response.(response.status) with
    | #Cohttp.Code.success_status -> Response.make response body
    | status ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      error_lwt (Unhandled_response_code { uri; status; body })

  let put ?ctx ?headers ?timeout ?body ?apm ?apm_tags uri =
    let headers = always_close headers in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:"PUT" ~uri)
         ~subtype:"PUT" ~action:"Skrest#put" (put ?ctx ~headers ?body)
      )
      uri

  let post_form ?ctx ?headers ~params uri =
    let%lwt (response, body) = C.post_form ?ctx ?headers ~params uri in
    match Cohttp.Response.(response.status) with
    | #Cohttp.Code.success_status -> Response.make response body
    | status ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      error_lwt (Unhandled_response_code { uri; status; body })

  let post_form ?ctx ?headers ?timeout ?apm ?apm_tags ~params uri =
    let headers = always_close headers in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:"POST" ~uri)
         ~subtype:"POST" ~action:"Skrest#post_form"
         (post_form ?ctx ~headers ~params)
      )
      uri

  let call ?ctx ?headers ?body meth uri =
    let%lwt (response, body) = C.call ?ctx ?headers ?body meth uri in
    match Cohttp.Response.(response.status) with
    | #Cohttp.Code.success_status -> Response.make response body
    | status ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      error_lwt (Unhandled_response_code { uri; status; body })

  let call ?ctx ?headers ?timeout ?body ?apm ?apm_tags meth uri =
    let meth_str = Cohttp.Code.string_of_method meth in
    let headers = always_close headers in
    wrap_unix_error ~timeout
      (wrap_with_transaction ?apm ?apm_tags
         ~name:(apm_name ~meth:meth_str ~uri)
         ~subtype:meth_str ~action:"Skrest#call"
         (call ?ctx ~headers ?body meth)
      )
      uri

  let retry ?wait:(wait_period = 0.) ~retries uri f () =
    let open Lwt in
    let rec helper retry =
      if retry = retries then
        f uri
      else
        f uri >>= function
        | Ok _ as ok -> Lwt.return ok
        | Error _ -> Lwt_unix.sleep wait_period >>= fun _ -> helper (retry + 1)
    in
    helper 0
end

module String_response_body = Make (String_t_response)
module Ignore_response_body = Make (Drain_t_response)
module Streaming_response_body = Make (Stream_t_response)

include Make (String_response)
