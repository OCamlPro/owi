(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Link.Make (struct
  type extern_func = Symbolic_extern.Func.t

  type extern_module = Symbolic_extern.Module.t

  let to_func_type = Symbolic_extern.Func.to_func_type

  type data = Symbolic_data.t

  let data_of_concrete data = data
end)

let get_memory _env _id : Symbolic_memory.t Symbolic_choice.t = assert false
(*
  let ( let* ) = Symbolic_choice.( let* ) in
  let* memories = Symbolic_choice.fold_state (fun state -> state.memories) in
  match Collection.find memories ~modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    let original = get_memory env id in
    let symbolic = Symbolic_memory.of_concrete ~modul ~id original in
    let* () = Symbolic_memory.replace symbolic in
    Symbolic_choice.return symbolic
*)

let get_table _env _id : Symbolic_table.t Symbolic_choice.t = assert false
(*
  let ( let* ) = Symbolic_choice.( let* ) in
  let* tables = Symbolic_choice.fold_state (fun state -> state.tables) in
  match Collection.find tables ~modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    let original = get_table env id in
    let symbolic = Symbolic_table.of_concrete ~modul ~id original in
    let* () = Symbolic_table.replace symbolic in
    Symbolic_choice.return symbolic
*)

let get_global _env _id : Symbolic_global.t Symbolic_choice.t = assert false
(*
  let ( let* ) = Symbolic_choice.( let* ) in
  let* globals = Symbolic_choice.fold_state (fun state -> state.globals) in
  match Collection.find globals ~modul ~id with
  | Some g -> Symbolic_choice.return g
  | None ->
    let original = get_global env id in
    let symbolic = Symbolic_global.of_concrete ~id original in
    let* () = Symbolic_global.replace symbolic in
    Symbolic_choice.return symbolic
    *)
