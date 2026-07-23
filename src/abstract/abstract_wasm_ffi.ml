(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Abstract_monad

(* The constraint is used here to make sure we don't forget to define one of
   the expected FFI functions, this whole file is further constrained such that
   if one function of M is unused in the FFI module below, an error will be
   displayed *)
module M :
  Wasm_ffi_intf.S0
    with type 'a t := 'a Abstract_monad.t
     and type memory := Abstract_memory.t
     and type i32 := Abstract_i32.t
     and type i64 := Abstract_i64.t
     and type f32 := Abstract_f32.t
     and type f64 := Abstract_f64.t
     and type v128 := Abstract_v128.t = struct
  let assume condition =
    map_state (fun ({ ctx; _ } as state) ->
      let condition = Abstract_i32.to_boolean ctx condition in
      match Abstract_domain.assume ctx condition with
      | None -> None
      | Some ctx -> Some { state with ctx } )

  let assert' condition = assume condition

  let exit _code = map_state (fun _ -> None)

  let abort () = map_state (fun _ -> None)

  let symbol_invisible_bool () =
    fold_state (fun { ctx; _ } -> Abstract_i32.unknown ctx)

  let symbol_i32 () = fold_state (fun { ctx; _ } -> Abstract_i32.unknown ctx)

  let symbol_i64 () = fold_state (fun { ctx; _ } -> Abstract_i64.unknown ctx)

  let symbol_f32 () = fold_state (fun { ctx; _ } -> Abstract_f32.unknown ctx)

  let symbol_f64 () = fold_state (fun { ctx; _ } -> Abstract_f64.unknown ctx)

  let symbol_v128 () =
    (* TODO *)
    assert false

  let symbol_range lo hi =
    let* v = fold_state (fun { ctx; _ } -> Abstract_i32.unknown ctx) in
    let* gt =
      fold_state (fun { ctx; _ } ->
        Abstract_i32.gt_s ctx v lo |> Abstract_i32.of_boolean ctx )
    in
    let* () = assume gt in
    let* lt =
      fold_state (fun { ctx; _ } ->
        Abstract_i32.lt_s ctx v hi |> Abstract_i32.of_boolean ctx )
    in
    let+ () = assume lt in
    v

  let alloc _m _base _size =
    Log.warn (fun m -> m "TODO : implement alloc");
    fold_state (fun { ctx; _ } -> Abstract_i32.unknown ctx)

  let free _m _ptr =
    Log.warn (fun m -> m "TODO : implement free");
    fold_state (fun { ctx; _ } -> Abstract_i32.unknown ctx)

  let in_replay_mode () = fold_state @@ fun { ctx; _ } -> Abstract_i32.zero ctx

  let print_char _c = return ()

  let cov_label_is_covered _id =
    fold_state @@ fun { ctx; _ } -> Abstract_i32.unknown ctx

  let cov_label_set _m _id _ptr = return ()

  let open_scope_null_terminated _m _ptr = return ()

  let open_scope_of_length _m _ptr _len = return ()

  let close_scope () = return ()
end

open M
open Abstract_extern.Func
open Abstract_extern.Func.Syntax

let owi =
  [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
  ; ("i64_symbol", Extern_func (unit ^->. i64, symbol_i64))
  ; ("f32_symbol", Extern_func (unit ^->. f32, symbol_f32))
  ; ("f64_symbol", Extern_func (unit ^->. f64, symbol_f64))
  ; ("v128_symbol", Extern_func (unit ^->. v128, symbol_v128))
  ; ("invisible_bool_symbol", Extern_func (unit ^->. i32, symbol_invisible_bool))
  ; ("range_symbol", Extern_func (i32 ^-> i32 ^->. i32, symbol_range))
  ; ("assume", Extern_func (i32 ^->. unit, assume))
  ; ("assert", Extern_func (i32 ^->. unit, assert'))
  ; ("in_replay_mode", Extern_func (unit ^->. i32, in_replay_mode))
  ; ("print_char", Extern_func (i32 ^->. unit, print_char))
  ; ( "cov_label_set"
    , Extern_func (memory 0 ^-> i32 ^-> i32 ^->. unit, cov_label_set) )
  ; ("cov_label_is_covered", Extern_func (i32 ^->. i32, cov_label_is_covered))
  ; ( "open_scope_null_terminated"
    , Extern_func (memory 0 ^-> i32 ^->. unit, open_scope_null_terminated) )
  ; ( "open_scope_of_length"
    , Extern_func (memory 0 ^-> i32 ^-> i32 ^->. unit, open_scope_of_length) )
  ; ("close_scope", Extern_func (unit ^->. unit, close_scope))
  ; ("alloc", Extern_func (memory 0 ^-> i32 ^-> i32 ^->. i32, alloc))
  ; ("dealloc", Extern_func (memory 0 ^-> i32 ^->. i32, free))
  ; ("abort", Extern_func (unit ^->. unit, abort))
  ; ("exit", Extern_func (i32 ^->. unit, exit))
  ]
