open Tezt_js

module _ = Skrest_common_tests.Make (Skrest_js)

let () =
  print_endline "Running JS tests.";
  Test.run ()
