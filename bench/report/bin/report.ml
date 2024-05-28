open Report

let () =
  if Array.length Sys.argv < 2 then
    Format.ksprintf failwith "usage: <%s> <FILE>" Sys.argv.(0)

let file = Fpath.v Sys.argv.(1)

let ok_or_fail = function
  | Error (`Msg msg) ->
    Format.eprintf "ERROR: %s@\n" msg;
    exit 1
  | Ok v -> v

let runs = Parse.from_file file

let output_dir = Fpath.v "./"

let () = Gen.full_report runs output_dir |> ok_or_fail
