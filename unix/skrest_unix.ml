module Backend : Skrest.Backend = struct
  type native_error = Unix.error

  let pp_native_error = Fmt.of_to_string Unix.error_message

  let sleep = Lwt_unix.sleep

  let inject_headers headers =
    Cohttp.Header.add_opt_unless_exists headers "connection" "close"

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
  open Backend_impl.String_t_response_body
  open struct
    let name ~meth_str ~uri =
      Fmt.(
        str "%s: %a" meth_str (option ~none:(any "<none>") string) (Uri.host uri)
      )

    let get_response v = v.Skrest.response
    let get_body v = v.Skrest.body
  end

  let wrap
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      ~get_response
      ~get_body
      ~meth
      f
      uri =
    let open Lwt in
    let r =
      match apm with
      | Some parent ->
        let meth_str = Cohttp.Code.string_of_method meth in
        let action =
          Fmt.str "Skrest_unix#%s" (String.lowercase_ascii meth_str)
        in
        let name = name ~meth_str ~uri in
        let span =
          Skapm.Span.make_span ~parent ~name ~type_:"Web Request"
            ~subtype:meth_str ~action ()
        in
        f uri >|= fun res ->
        let context =
          let open Skapm.Span.Context in
          let p = make ?tags:apm_tags in
          match res with
          | Ok skresp ->
            let resp = get_response skresp in
            let response =
              make_response
                ~headers:
                  (resp |> Cohttp.Response.headers |> Cohttp.Header.to_list)
                ~status_code:(resp |> Cohttp.Response.status)
                ()
            in
            let http = make_http ~meth ~url:uri ~response () in
            p ~http ()
          | Error _ -> p ()
        in
        let (_ : Skapm.Span.result) =
          Skapm.Span.finalize_and_send ~context span
        in
        res
      | None -> f uri
    in
    Lwt_result.map get_body r

  let head
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = head ?ctx ?headers ?timeout in
    wrap ?apm ?apm_tags ~meth:`HEAD ~get_body:Fun.id ~get_response:Fun.id run
      uri

  let get
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      ~(follow : int)
      (uri : Uri.t) =
    let run = get ?ctx ?headers ?timeout ~follow in
    wrap ?apm ?apm_tags ~get_body ~get_response ~meth:`GET run uri

  let delete
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = delete ?ctx ?headers ?timeout in
    wrap ?apm ?apm_tags ~get_body ~get_response ~meth:`DELETE run uri

  let patch
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(body : Cohttp_lwt.Body.t option)
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = patch ?ctx ?headers ?timeout ?body in
    wrap ?apm ?apm_tags ~get_body ~get_response ~meth:`PATCH run uri

  let post
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(body : Cohttp_lwt.Body.t option)
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      (uri : Uri.t) =
    let run = post ?ctx ?headers ?timeout ?body in
    wrap ?apm ?apm_tags ~get_body ~get_response ~meth:`POST run uri

  let post_form
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      ~(params : (string * string list) list)
      (uri : Uri.t) =
    let run = post_form ?ctx ?headers ?timeout ~params in
    wrap ?apm ?apm_tags ~get_body ~get_response ~meth:`POST run uri

  let call
      ?(ctx : ctx option)
      ?(headers : Cohttp.Header.t option)
      ?(timeout : float option)
      ?(body : Cohttp_lwt.Body.t option)
      ?(apm : Skapm.Span.parent option)
      ?(apm_tags : Skapm.Tag.t list option)
      (meth : Cohttp.Code.meth)
      (uri : Uri.t) =
    let run = call ?ctx ?headers ?timeout ?body meth in
    wrap ?apm ?apm_tags ~get_body ~get_response ~meth run uri
end
