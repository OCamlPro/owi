open Crowbarplus
open Owi.Types
open Owi.Types.Symbolic
open Syntax

let num_type = choose [ const I32; const I64 ]

let val_type =
  let+ nt = [ num_type ] in
  Num_type nt

let mut = choose [ const Const; const Var ]

let const_i32 =
  let+ i = [ int32 ] in
  I32_const i

let const_i64 =
  let+ i = [ int64 ] in
  I64_const i

let const_of_num_type = function
  | I32 -> const_i32
  | I64 -> const_i64
  | _ ->
    (* TODO: complete *)
    assert false

let const_of_val_type = function
  | Num_type nt -> const_of_num_type nt
  | _ ->
    (* TODO: complete *)
    assert false

let global_type = pair mut val_type

let param =
  let id : string option gen = const None in
  pair id val_type

let block_type =
  let param_type = list param in
  let result_type = list val_type in
  let typ = pair param_type result_type in
  let+ typ = [ typ ] in
  Arg.Bt_raw (None, typ)
