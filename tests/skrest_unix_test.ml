open Tezt

module _ = Skrest_common_tests.Make (Skrest_unix)

let () =
  print_endline "Running Unix tests.";
  Test.run ()
