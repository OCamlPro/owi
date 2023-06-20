open Owi

module type INTERPRET = sig
  type t

  val of_symbolic : Symbolic.modul -> t

  val run : t -> unit Result.t
end

module Owi_unoptimized : INTERPRET = struct
  type t = Symbolic.modul

  let of_symbolic = Fun.id

  let run modul =
    match Compile.until_simplify modul with
    | Error e -> failwith e
    | Ok simplified -> (
      match Typecheck.modul simplified with
      | Error e -> failwith e
      | Ok () -> (
        match Link.modul Link.empty_state ~name:None simplified with
        | Error e -> failwith e
        | Ok (regular, _link_state) -> Interpret.modul regular ) )
end

module Owi_optimized : INTERPRET = struct
  type t = Symbolic.modul

  let of_symbolic = Fun.id

  let run modul =
    match Compile.until_simplify modul with
    | Error e -> failwith e
    | Ok simplified -> (
      match Typecheck.modul simplified with
      | Error e -> failwith e
      | Ok () -> (
        let simplified = Optimize.modul simplified in
        match Link.modul Link.empty_state ~name:None simplified with
        | Error e -> failwith e
        | Ok (regular, _link_state) -> Interpret.modul regular ) )
end

module Reference : INTERPRET = struct
  type t = string

  let of_symbolic modul = Format.asprintf "%a" Symbolic.Pp.modul modul

  let run modul =
    let prefix = "owi_fuzzer_official" in
    let suffix = ".wast" in
    let tmp_file = Filename.temp_file prefix suffix in
    let chan = open_out tmp_file in
    let fmt = Format.formatter_of_out_channel chan in
    Format.pp_print_string fmt modul;
    close_out chan;
    let n = Sys.command @@ Format.sprintf "officialwasm %s" tmp_file in
    match n with
    | 0 -> Ok ()
    | 42 -> Error "trap"
    | n -> failwith (Format.sprintf "error %d" n)
end
