open Js_of_ocaml

module Backend : Skrest.Backend = struct
  type native_error = Js_error.t

  let pp_native_error = Fmt.of_to_string Js_error.to_string

  let sleep = Js_of_ocaml_lwt.Lwt_js.sleep

  let inject_headers = function
    | Some h -> h
    | None -> Cohttp.Header.init ()

  let handle_exn = function
    | Js_error.Exn err ->
      let message = Fmt.str "%a" pp_native_error err in
      Skrest.Connection_error (err, message)
    | exn ->
      let bt = Printexc.get_raw_backtrace () in
      Skrest.Trapped_exception (exn, bt)

  module Client = Cohttp_lwt_jsoo.Client
end

module Backend_impl = Skrest.Make_with_backend (Backend)

include Backend_impl

include Backend_impl.String_response_body
