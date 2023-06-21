open Core
open Encoding

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename -> In_channel.read_all filename

let parse_file file = get_contents file |> Run.parse_string

let command =
  Command.basic ~summary:"SMTLIB-like parser and interpreter"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command files =
       anon (sequence ("filename" %: Filename_unix.arg_type))
       (*and trial = flag "-t" no_arg ~doc:" run a built-in time trial" in*)
     in
     fun () ->
       match files with
       | [] ->
           let ast = parse_file "-" in
           Eval.start ast
       | _ ->
           let asts = List.map files ~f:Run.parse_file in
           List.iter asts ~f:(fun ast -> Eval.start ast))

let () = Command_unix.run ~version:"0.1" command
