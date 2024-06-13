(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Expr = Smtml.Expr
module Value = Symbolic_value
module Choice = Symbolic.P.Choice

let symbolic_extern_module :
  Symbolic.P.Extern_func.extern_func Link.extern_module =
  let sym_cnt = Atomic.make 0 in
  let symbol ty () : Value.int32 Choice.t =
    let id = Atomic.fetch_and_add sym_cnt 1 in
    let sym = Format.kasprintf (Smtml.Symbol.make ty) "symbol_%d" id in
    let sym_expr = Expr.mk_symbol sym in
    Choice.with_thread (fun thread ->
        Thread.add_symbol thread sym;
        match ty with
        | Ty_bitv 8 -> Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, sym_expr))
        | _ -> sym_expr )
  in
  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.add_pc c
  in
  let assume_positive_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.ge i Value.I32.zero in
    Choice.add_pc c
  in
  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.assertion c
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i8_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol (Ty_bitv 8)) )
    ; ( "i32_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol (Ty_bitv 32)) )
    ; ( "i64_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I64), symbol (Ty_bitv 64)) )
    ; ( "f32_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 F32), symbol (Ty_fp 32)) )
    ; ( "f64_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 F64), symbol (Ty_fp 64)) )
    ; ( "assume"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_i32) )
    ; ( "assume_positive_i32"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_positive_i32) )
    ; ( "assert"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { functions }

let summaries_extern_module :
  Symbolic.P.Extern_func.extern_func Link.extern_module =
  let open Expr in
  let abort () : unit Choice.t = Choice.add_pc @@ Value.Bool.const false in

  let i32 v : int32 Choice.t =
    match view v with
    | Val (Num (I32 v)) -> Choice.return v
    | _ ->
      Log.debug2 {|alloc: cannot allocate base pointer "%a"|} Expr.pp v;
      Choice.bind (abort ()) (fun () -> assert false)
  in
  let ptr v : int32 Choice.t =
    match view v with
    | Ptr (b, _) -> Choice.return b
    | _ ->
      Log.debug2 {|free: cannot fetch pointer base of "%a"|} Expr.pp v;
      Choice.bind (abort ()) (fun () -> assert false)
  in
  let alloc (base : Value.int32) (size : Value.int32) : Value.int32 Choice.t =
    Choice.bind (i32 base) (fun base ->
        Choice.with_thread (fun t ->
            let memories = Thread.memories t in
            Symbolic_memory.iter
              (fun tbl ->
                Symbolic_memory.ITbl.iter
                  (fun _ (m : Symbolic_memory.t) ->
                    Symbolic_memory.replace_size m base size )
                  tbl )
              memories;
            Expr.make (Ptr (base, Value.const_i32 0l)) ) )
  in
  let free (p : Value.int32) : unit Choice.t =
    Choice.bind (ptr p) (fun base ->
        Choice.with_thread (fun t ->
            let memories = Thread.memories t in
            Symbolic_memory.iter
              (fun tbl ->
                Symbolic_memory.ITbl.iter
                  (fun _ (m : Symbolic_memory.t) -> Symbolic_memory.free m base)
                  tbl )
              memories ) )
  in

  let exit (p : Value.int32) : unit Choice.t =
    ignore p;
    abort ()
  in
  let functions =
    [ ( "alloc"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), free) )
    ; ("abort", Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ; ( "exit"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), exit) )
    ]
  in
  { functions }

let ( let*/ ) (t : 'a Result.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  match t with Error e -> Choice.return (Error e) | Ok x -> f x

let link_state =
  lazy
    (let func_typ = Symbolic.P.Extern_func.extern_type in
     let link_state =
       Link.extern_module' Link.empty_state ~name:"symbolic" ~func_typ
         symbolic_extern_module
     in
     Link.extern_module' link_state ~name:"summaries" ~func_typ
       summaries_extern_module )

let run_binary_modul ~unsafe ~optimize (pc : unit Result.t Choice.t)
  (m : Binary.modul) =
  let*/ m = Cmd_utils.add_main_as_start m in
  let link_state = Lazy.force link_state in

  let*/ m, link_state =
    Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m
  in
  let m = Symbolic.convert_module_to_run m in
  let c = Interpret.SymbolicP.modul link_state.envs m in
  Choice.bind pc (fun r ->
      match r with Error _ -> Choice.return r | Ok () -> c )

let run_file ~unsafe ~optimize pc filename =
  let*/ m = Parse.guess_from_file filename in
  let*/ m =
    match m with
    | Either.Left (Either.Left text_module) ->
      Compile.Text.until_binary ~unsafe text_module
    | Either.Left (Either.Right _text_scrpt) ->
      Error (`Msg "can't run symbolic interpreter on a script")
    | Either.Right binary_module -> Ok binary_module
  in
  run_binary_modul ~unsafe ~optimize pc m

(* NB: This function propagates potential errors (Result.err) occurring
   during evaluation (OS, syntax error, etc.), except for Trap and Assert,
   which are handled here. Most of the computations are done in the Result
   monad, hence the let*. *)
let cmd profiling debug unsafe optimize workers no_stop_at_failure no_values
  deterministic_result_order (workspace : Fpath.t) solver files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  (* deterministic_result_order implies no_stop_at_failure *)
  let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in
  let* _created_dir = Bos.OS.Dir.create ~path:true ~mode:0o755 workspace in
  let pc = Choice.return (Ok ()) in
  let result = List.fold_left (run_file ~unsafe ~optimize) pc files in
  let thread : Thread.t = Thread.create () in
  let results = Symbolic_choice.run ~workers solver result thread in
  let print_bug = function
    | `ETrap (tr, model) ->
      Format.pp_std "Trap: %s@\n" (Trap.to_string tr);
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
    | `EAssert (assertion, model) ->
      Format.pp_std "Assert failure: %a@\n" Expr.pp assertion;
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
  in
  let rec print_and_count_failures count_acc results =
    match results () with
    | Seq.Nil -> Ok count_acc
    | Seq.Cons ((result, _thread), tl) ->
      let* is_err, model =
        let open Symbolic_choice in
        match result with
        | EAssert (assertion, model) ->
          print_bug (`EAssert (assertion, model));
          Ok (true, Some model)
        | ETrap (tr, model) ->
          print_bug (`ETrap (tr, model));
          Ok (true, Some model)
        | EVal (Ok ()) -> Ok (false, None)
        | EVal (Error e) -> Error e
      in
      let count_acc = if is_err then succ count_acc else count_acc in
      let* () =
        if not no_values then
          let model =
            match model with None -> Hashtbl.create 16 | Some m -> m
          in
          let testcase =
            List.sort compare (Smtml.Model.get_bindings model) |> List.map snd
          in
          Cmd_utils.write_testcase ~dir:workspace ~err:is_err testcase
        else Ok ()
      in
      if (not is_err) || no_stop_at_failure then
        print_and_count_failures count_acc tl
      else Ok count_acc
  in
  let results =
    if deterministic_result_order then
      results
      |> Seq.map (function (_, thread) as x ->
           (x, List.rev @@ Thread.breadcrumbs thread) )
      |> List.of_seq
      |> List.sort (fun (_, bc1) (_, bc2) ->
             List.compare Stdlib.Int32.compare bc1 bc2 )
      |> List.to_seq |> Seq.map fst
    else results
  in
  let* count = print_and_count_failures 0 results in
  if count > 0 then Error (`Found_bug count)
  else begin
    Format.pp_std "All OK";
    Ok ()
  end
