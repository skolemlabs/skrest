(** {1 "Easy" REST requests with cohttp} *)

(** {1 Request errors} *)
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

type ('a, 'native) result = ('a, [ `Skrest of 'native error ]) Stdlib.result

(** {1 Responses} *)

type 'body t = {
  response : Cohttp.Response.t;
      (** Response metadata (status code, headers, etc) *)
  body : 'body;  (** Response body *)
}
(** The contents of a response *)

(** Module type for creating different response handlers. *)
module type Response = sig
  type response

  val make :
    Cohttp_lwt.Response.t -> Cohttp_lwt.Body.t -> (response, _) result Lwt.t
end

module String_response : Response
module String_t_response : Response
module Drain_t_response : Response
module Stream_t_response : Response

(** Module type for creating different backend implemenations. *)
module type Backend = sig
  type native_error

  val pp_native_error : native_error Fmt.t

  val sleep : float -> unit Lwt.t

  val handle_exn : exn -> native_error error

  val inject_headers : Cohttp.Header.t option -> Cohttp.Header.t

  module Client : Cohttp_lwt.S.Client
end

module type Fetch = sig
  type response
  (** The type of a response *)

  type ctx
  (** The native context of the backend *)

  type native_error
  (** The native error type for a specific backend *)

  (** {1 The Verbs} *)

  val head :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    Uri.t ->
    (Cohttp.Response.t, native_error) result Lwt.t
  (** [head ?ctx ?headers uri] returns the result of a [HEAD] request to [uri]. *)

  val get :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    follow:int ->
    Uri.t ->
    (response, native_error) result Lwt.t
  (** [get ?ctx ?headers ~follow uri] returns the result of a [GET] request to
      [uri].

      @param follow specifies the maximum number of redirects to follow. *)

  val delete :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    Uri.t ->
    (response, native_error) result Lwt.t
  (** [delete ?ctx ?headers ~follow uri] returns the result of a [DELETE]
      request to [uri]. *)

  val patch :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Uri.t ->
    (response, native_error) result Lwt.t
  (** [patch ?ctx ?headers ?body uri] returns the result of a [PATCH] request to
      [uri].

      @param body is the [PATCH] body to use. There is no body by default. *)

  val post :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Uri.t ->
    (response, native_error) result Lwt.t
  (** [post ?ctx ?headers ?body uri] returns the result of a [POST] request to
      [uri].

      @param body is the [POST] body to use. There is no body by default. *)

  val put :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Uri.t ->
    (response, native_error) result Lwt.t
  (** [put ?ctx ?headers ?body uri] returns the result of a [PUT] request to
      [uri].

      @param body is the [PUT] body to use. There is no body by default. *)

  val post_form :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    params:(string * string list) list ->
    Uri.t ->
    (response, native_error) result Lwt.t
  (** [post_form ?ctx ?headers ~params uri] returns the result of a form [POST]
      request to [uri].

      @param params
        specifies a list of [(key, value)] pairs which represent the form
        elements to send. *)

  val call :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?timeout:float ->
    ?body:Cohttp_lwt.Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    (response, native_error) result Lwt.t
  (** [call ?ctx ?headers ?timeout ?body meth uri] makes a [meth] call to [uri].
      It is a more generic version of the other functions in this module. The
      more specific functions like {!get} and {!post} should be used when
      possible.

      @param body is the request body to send. There is no body by default. *)
end

module type Impl = sig
  type native_error

  type nonrec error = native_error error
  type nonrec 'a result = ('a, native_error) result
  type ctx

  (** {1 Streaming vs eagerly-consumed bodies} *)

  (** Functor for creating request engines with parameterized response types *)
  module Make_with_response (Response : Response) :
    Fetch
      with type response = Response.response
       and type native_error = native_error
       and type ctx = ctx

  val retry :
    ?wait:float ->
    retries:int ->
    Uri.t ->
    (Uri.t -> 'response result Lwt.t) ->
    unit ->
    'response result Lwt.t
  (** [retry ?wait ~retries uri f] retries [f] up to [retry] times. If [f]
      returns [Error(_)], the thread sleeps for [wait], then it is called again
      unless the number of times that [f] has been tried is equal to [retries].
      If [f] returns [Ok(_)], the value is returned. *)

  val retry_f :
    f:(float -> float option) ->
    Uri.t ->
    (Uri.t -> 'response result Lwt.t) ->
    'response result Lwt.t
  (** [retry ~f uri c] retries [c] until [f] returns [None]. If [c] returns
      [Error _], the thread sleeps for [f prev_wait] if it is [Some wait]. If
      [c] returns [Ok _], the value is returned. This is useful for implementing
      exponential backoff.*)

  module String_response_body :
    Fetch
      with type response = string
      with type native_error = native_error
       and type ctx = ctx
  module String_t_response_body :
    Fetch
      with type response = string t
      with type native_error = native_error
       and type ctx = ctx
  module Drain_t_response_body :
    Fetch
      with type response = unit t
      with type native_error = native_error
       and type ctx = ctx
  module Stream_t_response_body :
    Fetch
      with type response = Cohttp_lwt.Body.t t
      with type native_error = native_error
       and type ctx = ctx

  (** {2 Printing and converting errors} *)

  val pp_error : Format.formatter -> [< `Skrest of error ] -> unit
  val open_error : 'a result -> ('a, [> `Skrest of error ]) Stdlib.result
  val error_to_msg : 'a result -> ('a, [> `Msg of string ]) Stdlib.result
end

(** Functor to create implemenations based on a backend. * i.e.
    cohttp_lwt_jsoo/cohttp_lwt_unix *)
module Make_with_backend (Backend : Backend) :
  Impl
    with type native_error = Backend.native_error
     and type ctx = Backend.Client.ctx
