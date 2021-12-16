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

include Backend_impl.String_response_body
