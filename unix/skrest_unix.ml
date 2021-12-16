module Backend : Skrest.Backend = struct
  type native_error = Unix.error

  let pp_native_error = Fmt.of_to_string Unix.error_message

  let sleep = Lwt_unix.sleep

  let handle_exn = function
    | Unix.Unix_error (err, func, arg) ->
      let message =
        Fmt.str "%s from %s(%s)" (Unix.error_message err) func arg
      in
      Skrest.Connection_error (err, message)
    | exn ->
      let bt = Printexc.get_raw_backtrace () in
      Skrest.Trapped_exception (exn, bt)

  module Client = Cohttp_lwt_unix.Client
end

module Backend_impl = Skrest.Make_with_backend (Backend)

include Backend_impl

include Backend_impl.String_response_body

module Apm = struct
  let name ~methd ~uri = Fmt.str "%s: %s" methd (Uri.to_string uri)

  let wrap
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
      let (_ : Skapm.Span.result) = Skapm.Span.finalize_and_send span in
      res
    | None -> f x

  let head
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Transaction.t option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = head ?ctx ?headers ?timeout in
    wrap ?apm ?apm_tags ~name:(name ~methd:"HEAD" ~uri) ~subtype:"HEAD"
      ~action:"Skrest_unix#head" run

  let get
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Transaction.t option)
      ?(apm_tags : Skapm.Tag.t list option)
      ~(follow : int)
      (uri : Uri.t) =
    let run = get ?ctx ?headers ?timeout ~follow in
    wrap ?apm ?apm_tags ~name:(name ~methd:"GET" ~uri) ~subtype:"GET"
      ~action:"Skrest_unix#get" run

  let delete
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Transaction.t option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = delete ?ctx ?headers ?timeout in
    wrap ?apm ?apm_tags
      ~name:(name ~methd:"DELETE" ~uri)
      ~subtype:"DELETE" ~action:"Skrest_unix#delete" run

  let patch
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(body : Cohttp_lwt.Body.t option)
      ?(apm : Skapm.Transaction.t option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = patch ?ctx ?headers ?timeout ?body in
    wrap ?apm ?apm_tags ~name:(name ~methd:"PATCH" ~uri) ~subtype:"PATCH"
      ~action:"Skrest_unix#patch" run

  let post
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(body : Cohttp_lwt.Body.t option)
      ?(apm : Skapm.Transaction.t option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = post ?ctx ?headers ?timeout ?body in
    wrap ?apm ?apm_tags ~name:(name ~methd:"POST" ~uri) ~subtype:"POST"
      ~action:"Skrest_unix#post" run

  let post_form
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Transaction.t option)
      ?(apm_tags : Skapm.Tag.t list option)
      ~(params : (string * string list) list)
      (uri : Uri.t) =
    let run = post_form ?ctx ?headers ?timeout ~params in
    wrap ?apm ?apm_tags ~name:(name ~methd:"POST" ~uri) ~subtype:"POST"
      ~action:"Skrest_unix#post_form" run

  let call
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(body : Cohttp_lwt.Body.t option)
      ?(apm : Skapm.Transaction.t option)
      ?(apm_tags : Skapm.Tag.t list option)
      (methd : Cohttp.Code.meth)
      (uri : Uri.t) =
    let run = call ?ctx ?headers ?timeout ?body methd in
    let methd_str = Cohttp.Code.string_of_method methd in
    wrap ?apm ?apm_tags
      ~name:(name ~methd:methd_str ~uri)
      ~subtype:methd_str ~action:"Skrest_unix#call" run
end
