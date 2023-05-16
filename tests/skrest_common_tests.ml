open Tezt_core
open Base

module J_util = Yojson.Safe.Util

let ( let* ) = Lwt.bind

let httpbin = Option.(value ~default:(Uri.of_string "http://httpbin.org") (map Uri.of_string (Sys.getenv_opt "HTTPBIN")))
let pp_json = Yojson.Safe.pretty_print ~std:false

module Make (M : Skrest.Impl) = struct
  let pp_err = M.pp_error
  module Skrest_impl = M.String_response_body

  let () =
    Test.register ~__FILE__ ~title:"GET 200" ~tags:[ "get"; "200"; "ok" ]
    @@ fun () ->
    let uri = Uri.with_path httpbin "/status/200" in
    let* result = Skrest_impl.get ~follow:0 uri in
    match result with
    | Ok _ -> unit
    | Error e -> Test.fail ~__LOC__ "expected [Ok _] (200), got %a" pp_err e

  let () =
    Test.register ~__FILE__ ~title:"GET 404" ~tags:[ "get"; "404"; "error" ]
    @@ fun () ->
    let uri = Uri.with_path httpbin "/status/404" in
    let* result = Skrest_impl.get ~follow:0 uri in
    match result with
    | Error (`Skrest (Unhandled_response_code { status = `Not_found; _ })) ->
      unit
    | Error e ->
      Test.fail ~__LOC__ "expected [Error `Not_found], got %a" pp_err e
    | Ok _ -> Test.fail ~__LOC__ "expected [Error `Not_found], got Ok _"

  let () =
    Test.register ~__FILE__ ~title:"GET headers"
      ~tags:[ "get"; "ok"; "headers" ]
    @@ fun () ->
    let uri = Uri.with_path httpbin "/headers" in
    let headers = Cohttp.Header.init_with "Skrest" "unix" in
    let* result = Skrest_impl.get ~follow:0 ~headers uri in
    match result with
    | Ok txt ->
      let json = Yojson.Safe.from_string txt in
      let headers = J_util.member "headers" json in
      let skrest = J_util.member "Skrest" headers in
      ( match skrest with
      | `String "unix" -> unit
      | json ->
        Test.fail ~__LOC__
          {|expected header "Skrest" to contain "unix", got %a|} pp_json json
      )
    | Error e -> Test.fail "headers request failed with %a" pp_err e

  let () =
    Test.register ~__FILE__ ~title:"retry_f" ~tags:[ "get"; "retry_f" ]
    @@ fun () ->
    let uri = Uri.with_path httpbin "/status/500" in
    let tried = ref 0 in
    let waited = ref 0. in
    let f' = function
      | 0. -> Some 0.01
      | 0.01 -> Some 0.02
      | 0.02 -> Some 0.03
      | _ -> None
    in
    let f d =
      let w = f' d in
      incr tried;
      Option.iter (fun w -> waited := !waited +. w) w;
      w
    in
    let* result = M.retry_f ~f uri (Skrest_impl.get ~follow:0) in
    match result with
    | Ok _ -> Test.fail ~__LOC__ "expected [Error 500], got [Ok _]"
    | Error _ ->
      if !tried <> 4 || !waited <> 0.06 then
        Test.fail ~__LOC__
          "failed less than 4 times (%d) or waited less than 0.06s (%f)" !tried
          !waited
      else
        unit
end
