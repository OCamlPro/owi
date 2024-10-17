(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

module M (Model : sig
  val model : Concrete_value.t Array.t
end) :
  Wasm_ffi_intf.S0
    with type 'a t = 'a
     and type memory = Concrete_memory.t
     and module Value = V = struct
  type 'a t = 'a

  module Value = V

  type memory = Concrete_memory.t

  let model = Model.model

  let next =
    let next = ref ~-1 in
    fun () ->
      incr next;
      !next

  let assume_i32 _ = ()

  let assume_positive_i32 _ = ()

  let assert_i32 n = assert (not @@ Prelude.Int32.equal n 0l)

  let symbol_i8 () = match model.(next ()) with I32 n -> n | _ -> assert false

  let symbol_char = symbol_i8

  let symbol_i32 = symbol_i8

  let symbol_i64 _ = assert false

  let symbol_f32 _ = assert false

  let symbol_f64 _ = assert false

  let abort () = assert false

  let alloc _ _ _ = assert false

  let free _ _ = assert false

  let exit _ = assert false
end

let run_file ~unsafe ~optimize filename (module Model) =
  let module M = M (Model) in
  let* m = Compile.File.until_binary_validate ~unsafe filename in
  let* m = Cmd_utils.add_main_as_start m in
  let link_state = Link.empty_state in

  let* m, link_state =
    Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m
  in
  let c = Interpret.Concrete.modul link_state.envs m in
  c

let parse_replay_file _replay_file : 'a Array.t = assert false

let cmd profiling debug unsafe optimize (replay_file : Fpath.t) file =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let model = parse_replay_file replay_file in
  let module Model : sig
    val model : Concrete_value.t Array.t
  end = struct
    let model = model
  end in
  let _result = run_file ~unsafe ~optimize file (module Model) in
  Fmt.pr "All OK@.";
  Ok ()
