open Js_of_ocaml

module Backend : Skrest.Backend = struct
  type native_error = Js.error Js.t

  let pp_native_error = Fmt.of_to_string Js.string_of_error
  
  let sleep = Js_of_ocaml_lwt.Lwt_js.sleep

  let handle_exn = function
    | Js.Error err ->
      let message =
        Fmt.str "%a" pp_native_error err in
      Skrest.Connection_error (err, message)
    | exn ->
      let bt = Printexc.get_raw_backtrace () in
      Skrest.Trapped_exception (exn, bt)

  module Client = Cohttp_lwt_jsoo.Client
end

module Backend_impl = Skrest.Make_with_backend (Backend)

include Backend_impl.String_response_body

