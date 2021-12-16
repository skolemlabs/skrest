open Skrest_js

let ( let* ) = Lwt.bind

let () =
  (let uri = Uri.of_string "https://httpbin.org/anything" in
   let* resp = get ~follow:0 uri in
   match resp with
   | Ok str ->
     print_endline str;
     Lwt.return_unit
   | Error err ->
     Format.printf "Error: %a" pp_error err;
     Lwt.return_unit
  )
  |> ignore
