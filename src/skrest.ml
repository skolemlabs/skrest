open Rresult

type 'native error =
  | Too_many_redirects
  | Unexpected_redirect_body_content of Uri.t
  | Missing_redirect_header of Uri.t
  | Unhandled_response_code of unhandled_response_code
  | Connection_error of 'native * string
  | Trapped_exception of exn * Printexc.raw_backtrace
  | Timed_out

and unhandled_response_code = {
  uri : Uri.t;
  status : Cohttp.Code.status_code;
  body : string;
}

type nonrec ('a, 'native) result = ('a, [ `Skrest of 'native error ]) result

let error e = R.error @@ `Skrest e
let error_lwt e = Lwt.return @@ error e

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
      let%lwt () = Cohttp_lwt.Body.drain_body b in
      Lwt.return { response; body }
  | Drain ->
    fun b ->
      let%lwt () = Cohttp_lwt.Body.drain_body b in
      Lwt.return { response; body = () }
  | Stream -> fun b -> Lwt.return { response; body = b }

let default_timeout = ref 30.0

module type Response = sig
  type response

  val make :
    Cohttp_lwt.Response.t -> Cohttp_lwt.Body.t -> (response, _) result Lwt.t
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

module type Backend = sig
  type native_error

  val pp_native_error : native_error Fmt.t

  val sleep : float -> unit Lwt.t

  val handle_exn : exn -> native_error error

  val inject_headers : Cohttp.Header.t option -> Cohttp.Header.t
  (** [inject_headers h] adds platform-specific headers to [h] *)

  module Client : Cohttp_lwt.S.Client
end

let wrap_error ~sleep ~handle_exn ~timeout f x =
  let timeout =
    let duration =
      match timeout with
      | None -> !default_timeout
      | Some duration -> duration
    in
    let%lwt () = sleep duration in
    error_lwt Timed_out
  in
  try%lwt Lwt.pick [ f x; timeout ] with
  | exn -> error_lwt @@ handle_exn exn

module type Impl = sig
  type native_error

  type nonrec error = native_error error
  type nonrec 'a result = ('a, native_error) result
  type ctx

  module type S = sig
    type response

    val head :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      Uri.t ->
      Cohttp.Response.t result Lwt.t

    val get :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      follow:int ->
      Uri.t ->
      response result Lwt.t

    val delete :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      Uri.t ->
      response result Lwt.t

    val patch :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
      Uri.t ->
      response result Lwt.t

    val post :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
      Uri.t ->
      response result Lwt.t

    val put :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
      Uri.t ->
      response result Lwt.t

    val post_form :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      params:(string * string list) list ->
      Uri.t ->
      response result Lwt.t

    val call :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
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

  module Make_with_response (Response : Response) :
    S with type response = Response.response

  module String_response_body : S with type response = string
  module String_t_response_body : S with type response = string t
  module Drain_t_response_body : S with type response = unit t
  module Stream_t_response_body : S with type response = Cohttp_lwt.Body.t t

  val pp_error : Format.formatter -> [< `Skrest of error ] -> unit
  val open_error : 'a result -> ('a, [> `Skrest of error ]) Stdlib.result
  val error_to_msg : 'a result -> ('a, [> `Msg of string ]) Stdlib.result
end

module Make_with_backend (Backend : Backend) :
  Impl
    with type native_error = Backend.native_error
     and type ctx = Backend.Client.ctx = struct
  type native_error = Backend.native_error
  type nonrec error = native_error error
  type nonrec 'a result = ('a, Backend.native_error) result

  module C = Backend.Client
  type ctx = C.ctx

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
    | Connection_error (err, message) ->
      Fmt.pf fmt "Connection error (%a) %s" Backend.pp_native_error err message
    | Trapped_exception (exn, _backtrace) ->
      Fmt.pf fmt "Trapped cohttp exception %s" (Printexc.to_string exn)
    | Timed_out -> Fmt.string fmt "Request timed out"

  let open_error = function
    | Ok _ as o -> o
    | Error (`Skrest _) as e -> e

  let error_to_msg = function
    | Ok _ as o -> o
    | Error err -> Error (`Msg (Fmt.str "%a" pp_error err))

  module type S = sig
    type response

    val head :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      Uri.t ->
      Cohttp.Response.t result Lwt.t

    val get :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      follow:int ->
      Uri.t ->
      response result Lwt.t

    val delete :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      Uri.t ->
      response result Lwt.t

    val patch :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
      Uri.t ->
      response result Lwt.t

    val post :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
      Uri.t ->
      response result Lwt.t

    val put :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
      Uri.t ->
      response result Lwt.t

    val post_form :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      params:(string * string list) list ->
      Uri.t ->
      response result Lwt.t

    val call :
      ?ctx:ctx ->
      ?headers:Cohttp.Header.t ->
      ?timeout:float ->
      ?body:Cohttp_lwt.Body.t ->
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

    val retry_f :
      f:(float -> float option) ->
      Uri.t ->
      (Uri.t -> response result Lwt.t) ->
      response result Lwt.t
  end

  module Make_with_response (Response : Response) :
    S with type response = Response.response = struct
    type response = Response.response

    let make_response r b =
      let promise = Response.make r b in
      Lwt.on_cancel promise (fun () ->
          Lwt.async (fun () -> Cohttp_lwt.Body.drain_body b)
      );
      promise

    let head ?ctx ?headers ?timeout uri =
      let run uri =
        let headers = Backend.inject_headers headers in
        let%lwt result = C.head ?ctx ~headers uri in
        Lwt.return @@ Ok result
      in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        run uri

    let rec get ?ctx ?headers ~follow uri =
      let%lwt (response, body) = C.get ?ctx ?headers uri in
      match Cohttp.Response.(response.status) with
      | #Cohttp.Code.success_status -> make_response response body
      | `Found
      | `Temporary_redirect ->
        if follow > 0 then
          if%lwt Cohttp_lwt.Body.is_empty body then (
            (* Body is empty, does not need to be drained. *)
            let response_headers = Cohttp.Response.headers response in
            match Cohttp.Header.get_location response_headers with
            | Some uri -> get ?ctx ?headers ~follow:(pred follow) uri
            | None -> error_lwt (Missing_redirect_header uri)
          ) else (
            (* Unexpected body content, must drain *)
            let%lwt () = Cohttp_lwt.Body.drain_body body in
            error_lwt (Unexpected_redirect_body_content uri)
          )
        else (
          (* Possibly contains body, optimisitically drain *)
          let%lwt () = Cohttp_lwt.Body.drain_body body in
          error_lwt Too_many_redirects
        )
      | status ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        error_lwt (Unhandled_response_code { uri; status; body })

    let get ?ctx ?headers ?timeout ~follow uri =
      let headers = Backend.inject_headers headers in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        (get ?ctx ~headers ~follow)
        uri

    let delete ?ctx ?headers uri =
      let%lwt (response, body) = C.delete ?ctx ?headers uri in
      match Cohttp.Response.(response.status) with
      | #Cohttp.Code.success_status -> make_response response body
      | status ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        error_lwt (Unhandled_response_code { uri; status; body })

    let delete ?ctx ?headers ?timeout uri =
      let headers = Backend.inject_headers headers in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        (delete ?ctx ~headers) uri

    let patch ?ctx ?headers ?body uri =
      let%lwt (response, body) = C.patch ?ctx ?headers ?body uri in
      match Cohttp.Response.(response.status) with
      | #Cohttp.Code.success_status -> make_response response body
      | status ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        error_lwt (Unhandled_response_code { uri; status; body })

    let patch ?ctx ?headers ?timeout ?body uri =
      let headers = Backend.inject_headers headers in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        (patch ?ctx ~headers ?body)
        uri

    let post ?ctx ?headers ?body uri =
      let%lwt (response, body) = C.post ?ctx ?headers ?body uri in
      match Cohttp.Response.(response.status) with
      | #Cohttp.Code.success_status -> make_response response body
      | status ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        error_lwt (Unhandled_response_code { uri; status; body })

    let post ?ctx ?headers ?timeout ?body uri =
      let headers = Backend.inject_headers headers in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        (post ?ctx ~headers ?body) uri

    let put ?ctx ?headers ?body uri =
      let%lwt (response, body) = C.put ?ctx ?headers ?body uri in
      match Cohttp.Response.(response.status) with
      | #Cohttp.Code.success_status -> make_response response body
      | status ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        error_lwt (Unhandled_response_code { uri; status; body })

    let put ?ctx ?headers ?timeout ?body uri =
      let headers = Backend.inject_headers headers in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        (put ?ctx ~headers ?body) uri

    let post_form ?ctx ?headers ~params uri =
      let%lwt (response, body) = C.post_form ?ctx ?headers ~params uri in
      match Cohttp.Response.(response.status) with
      | #Cohttp.Code.success_status -> make_response response body
      | status ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        error_lwt (Unhandled_response_code { uri; status; body })

    let post_form ?ctx ?headers ?timeout ~params uri =
      let headers = Backend.inject_headers headers in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        (post_form ?ctx ~headers ~params)
        uri

    let call ?ctx ?headers ?body meth uri =
      let%lwt (response, body) = C.call ?ctx ?headers ?body meth uri in
      match Cohttp.Response.(response.status) with
      | #Cohttp.Code.success_status -> make_response response body
      | status ->
        let%lwt body = Cohttp_lwt.Body.to_string body in
        error_lwt (Unhandled_response_code { uri; status; body })

    let call ?ctx ?headers ?timeout ?body meth uri =
      let headers = Backend.inject_headers headers in
      wrap_error ~sleep:Backend.sleep ~handle_exn:Backend.handle_exn ~timeout
        (call ?ctx ~headers ?body meth)
        uri

    let retry ?wait:(wait_period = 0.) ~retries uri f () =
      let open Lwt in
      let rec helper retry =
        if retry = retries then
          f uri
        else
          f uri >>= function
          | Ok _ as ok -> Lwt.return ok
          | Error _ -> Backend.sleep wait_period >>= fun _ -> helper (retry + 1)
      in
      helper 0

    let retry_f ~f uri c =
      let open Lwt in
      let rec helper wait =
        Backend.sleep wait >>= fun () ->
        c uri >>= function
        | Ok _ as ok -> Lwt.return ok
        | Error _ as e ->
          ( match f wait with
          | Some wait -> helper wait
          | None -> Lwt.return e
          )
      in
      helper 0.
  end

  module String_response_body = Make_with_response (String_response)
  module String_t_response_body = Make_with_response (String_t_response)
  module Drain_t_response_body = Make_with_response (Drain_t_response)
  module Stream_t_response_body = Make_with_response (Stream_t_response)
end
