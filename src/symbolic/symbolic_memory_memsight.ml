(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

[@@@warning "-69"]

module Memsight_backend = struct
  open Smtml
  open Symbolic_choice_without_memory
  include Symbolic_memory_base

  type address = Symbolic_value.int32

  type record =
    { address : address
    ; value : Symbolic_value.int32
    ; condition : Symbolic_value.vbool
    ; time : int
    }

  type t =
    { mutable records : record list
    ; mutable time : int
    }

  let make () = { records = []; time = 0 }

  let clone { records; time } = { records; time }

  let address a = return a

  let address_i32 i = Symbolic_value.const_i32 i

  let load_byte mem addr =
    let open Symbolic_value in
    let v = Expr.value (Num (I8 0)) in
    List.fold_right
      (fun r acc ->
        Bool.select_expr
          (Bool.and_ (I32.eq addr r.address) r.condition)
          ~if_true:r.value ~if_false:acc )
      mem.records v

  let loadn mem addr n =
    let open Symbolic_value in
    let rec loop i acc =
      if i = n then acc
      else
        let addr' = I32.add addr (const_i32 (Int32.of_int i)) in
        let byte = load_byte mem addr' in
        loop (i + 1) (concat i ~msb:byte ~lsb:acc)
    in
    loop 1 (load_byte mem addr)

  let storen mem addr v n =
    let open Symbolic_value in
    mem.time <- succ mem.time;
    for i = n - 1 downto 0 do
      let record =
        { address = I32.add addr (const_i32 (Int32.of_int i))
        ; value = extract v i
        ; condition = Bool.const true
        ; time = mem.time
        }
      in
      mem.records <- record :: mem.records
    done

  let validate_address _m a = return (Ok a)

  let free _ _ = return ()

  let realloc _m ~ptr ~size:_ =
    let+ base = select_i32 ptr in
    Expr.ptr base (Symbolic_value.const_i32 0l)

  let pp_record fmt { address; value; condition; time } =
    Fmt.pf fmt "%a, %a, %a, %d" Expr.pp address Expr.pp value Expr.pp condition
      time

  let pp fmt { time; records; _ } =
    Fmt.pf fmt "Time: %d@;" time;
    Fmt.pf fmt "Records:@;  @[<v 2>%a@]"
      (Fmt.list (Fmt.parens pp_record))
      records
end

module Memsight_backend' : Symbolic_memory_intf.M = Memsight_backend

include Symbolic_memory_make.Make (Memsight_backend)
