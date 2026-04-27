open Syntax
module Stack = Abs_stack

type t =
  | I32
  | I64

type v =
  | I32 of int32
  | I64 of int64

type sigma = v Stack.t

type bf =
  | Block
  | Loop
  | Func

type bt =
  { arg : t list
  ; result : t list
  }

type l =
  { form : bf
  ; ty : bt
  ; code : Binary.expr
  }

module Map = Map.Make (Int)

type e = v Map.t

type states = (l option * sigma * e) list

(*=========================================================================*)

let pp_t fmt : t -> unit = function
  | I32 -> Fmt.pf fmt "i32"
  | I64 -> Fmt.pf fmt "i64"

let pp_l fmt { form; ty; code } =
  Fmt.pf fmt "(%s (%a)->(%a) %s)"
    (match form with Block -> "b" | Loop -> "l" | Func -> "f")
    (Fmt.list ~sep:(Fmt.any ", ") pp_t)
    ty.arg
    (Fmt.list ~sep:(Fmt.any ", ") pp_t)
    ty.result
    (if List.length code > 0 then "I" else "[]")

let pp_v fmt = function
  | I32 i -> Fmt.pf fmt "(i32 %ld)" i
  | I64 i -> Fmt.pf fmt "(i64 %Ld)" i

let pp_map fmt map =
  let bindings = Map.bindings map in
  Fmt.pf fmt "@[<hov>{%a}@]"
    (Fmt.list ~sep:(Fmt.any ",@ ") (fun fmt (k, v) ->
       Fmt.pf fmt "%d->%a" k pp_v v ) )
    bindings

let print_state (states : states) =
  Fmt.pr "%a@."
    (Fmt.list ~sep:(Fmt.any " ::@; ") (fun fmt (l, sigma, e) ->
       Fmt.pf fmt "@[<hov>ℓ:%a;@;σ:[%a];@;ρ:%a@]"
         (Fmt.option pp_l ~none:(Fmt.any "∅"))
         l
         (Stack.pp pp_v) sigma pp_map e ) )
    states

let rec input_loop state =
  match In_channel.input_line In_channel.stdin with
  | None | Some "n" | Some "" -> ()
  | Some "p" ->
    print_state state;
    input_loop state
  | Some "q" -> exit 0
  | _ ->
    Fmt.pr "Input should be <cr>|n|p@.";
    input_loop state

let option_get = function Some x -> x | None -> assert false

let func_type_to_bt ((params, results) : Binary.func_type) =
  let val_type_to_bt : Binary.val_type -> t = function
    | Num_type Text.I32 -> I32
    | Num_type Text.I64 -> I64
    | Ref_type _ -> Fmt.failwith "we don't handle refs for now"
    | _ -> Fmt.failwith "not handled yet"
  in
  let params = List.map snd params in
  { arg = List.map val_type_to_bt params
  ; result = List.map val_type_to_bt results
  }

(*=========================================================================*)

module Binop = struct
  (* let run_op op e1 e2 = *)
  (*   match op with *)
  (*   | `Plus -> *)
  (*       match e1 with *)
  (*       | I32  -> Int32.add *)
  (*       | I64 -> Int64.add *)

  (* let binop stack op = *)
  (*   let e1, e2, stack = Stack.pop_2 stack in *)
  (*   match e1, e2 with *)
  (*   | I64 e1, I64 e2 -> *)
  (*   | I32 e1, I32 e2 -> *)

  (* let add state size = *)
  (*   assert false *)

end

let eval_ibinop (state : states) (size : Text.nn) (op : Text.ibinop) :
  states Result.t =
  match state with
  | (l, sigma, rho) :: state' -> begin
    match size with
    | S32 -> begin
      let int32_op =
        match op with
        | Text.Add -> Int32.add
        | Text.Sub -> Int32.sub
        | Text.Mul -> Int32.mul
        | Text.Div _sx -> Int32.div
        | _ -> Fmt.failwith "TODO"
      in
      match sigma with
      | I32 n1 :: I32 n2 :: sigma ->
        let n = I32 (int32_op n1 n2) in
        Ok ((l, n :: sigma, rho) :: state')
      | _ -> Fmt.error_msg "ibinop: (s32) malformed stack"
    end
    | S64 -> begin
      let int64_op =
        match op with
        | Text.Add -> Int64.add
        | Text.Sub -> Int64.sub
        | Text.Mul -> Int64.mul
        | Text.Div _sx -> Int64.div
        | _ -> Fmt.failwith "TODO"
      in
      match sigma with
      | I64 n1 :: I64 n2 :: sigma ->
        let n = I64 (int64_op n1 n2) in
        Ok ((l, n :: sigma, rho) :: state')
      | _ -> Fmt.error_msg "ibinop: (s64) malformed stack"
    end
  end
  | [] -> Fmt.error_msg "empty state"

let eval_irelop (state : states) (size : Text.nn) (op : Text.irelop) =
  match state with
  | (l, sigma, rho) :: state' -> begin
    match size with
    | Text.S32 -> (
      let op =
        match op with
        | Text.Eq -> Int32.eq
        | Ne -> Int32.ne
        | Lt s -> ( match s with U -> Int32.lt_u | S -> Int32.lt )
        | Gt s -> (
          fun x y ->
            match s with U -> not @@ Int32.le_u x y | S -> not @@ Int32.le x y )
        | Le s -> ( match s with U -> Int32.le_u | S -> Int32.le )
        | Ge s -> (
          fun x y ->
            match s with U -> not @@ Int32.lt_u x y | S -> not @@ Int32.lt x y )
      in
      match sigma with
      | I32 n1 :: I32 n2 :: sigma' ->
        let to_int32 x = match x with false -> 0l | true -> 1l in
        Ok ((l, I32 (op n1 n2 |> to_int32) :: sigma', rho) :: state')
      | _ -> assert false )
    | S64 -> (
      let op =
        match op with
        | Text.Eq -> Int64.eq
        | Ne -> Int64.ne
        | Lt s -> ( match s with U -> Int64.lt_u | S -> Int64.lt )
        | Gt s -> (
          fun x y ->
            match s with U -> not @@ Int64.le_u x y | S -> not @@ Int64.le x y )
        | Le s -> ( match s with U -> Int64.le_u | S -> Int64.le )
        | Ge s -> (
          fun x y ->
            match s with U -> not @@ Int64.lt_u x y | S -> not @@ Int64.lt x y )
      in
      match sigma with
      | I64 n1 :: I64 n2 :: sigma' ->
        let to_int64 x = match x with false -> 0L | true -> 1L in
        Ok ((l, I64 (op n1 n2 |> to_int64) :: sigma', rho) :: state')
      | _ -> assert false )
  end
  | [] -> assert false

let i32_binop stack op =
  let v1, v2, stack = Stack.pop_2 stack in
  match (v1, v2) with
  | I32 i1, I32 i2 -> Stack.push stack (I32 (op i1 i2))
  | _ -> assert false

  

let eval_i32 (l, sigma, rho) : Binary.i32_instr -> _ = function
  | Binary.Const i -> 
      let sigma = Stack.push sigma (I32 i) in
      (l, sigma, rho)
  | Add -> 
      let sigma = (i32_binop sigma (Int32.add)) in
      (l, sigma, rho)
  | Sub -> (
      let sigma = (i32_binop sigma (Int32.sub)) in
      (l, sigma, rho))
  | _ -> assert false

let eval_i64 (l, sigma, rho) : Binary.i32_instr -> _ = function
  | _ -> assert false

let rec eval_instr ~no_input (instrs : Binary.instr Annotated.t list)
  (states : states) : states Result.t =
  let* (l, sigma, rho) as state, states' =
    match states with
    | (l, sigma, rho) :: state' -> Ok ((l, sigma, rho), state')
    | [] -> Fmt.error_msg "eval_instr: empty state"
  in
  match instrs with
  | i :: instrs -> begin
    if no_input then (
      let instr_str = Fmt.str "%a" (Binary.pp_instr ~short:false) i.raw in
      Fmt.pr "# %-40s" instr_str;
      print_state states )
    else Fmt.pr "# %a@." (Binary.pp_instr ~short:false) i.raw;
    let* res, res_instrs =
      begin match i.raw with
      | Binary.I32 instr -> Ok (eval_i32 state instr :: states', instrs)
      | Binary.I64 instr -> Ok (eval_i64 state instr :: states', instrs)
      | Local_get i -> (
        match Map.find_opt i rho with
        | None -> Fmt.error_msg "local.get: local %i is not set" i
        | Some v -> Ok ((l, v :: sigma, rho) :: states', instrs) )
      | Local_set i -> (
        match sigma with
        | v :: sigma ->
          let rho = Map.add i v rho in
          Ok ((l, sigma, rho) :: states', instrs)
        | _ -> Fmt.error_msg "local.set: empty stack" )
      | Local_tee i -> (
        match sigma with
        | v :: sigma ->
          let rho = Map.add i v rho in
          Ok ((l, v :: sigma, rho) :: states', instrs)
        | _ -> Fmt.error_msg "local.tee: empty stack" )
      | Drop -> (
        match sigma with
        | _ :: sigma -> Ok ((l, sigma, rho) :: states', instrs)
        | [] -> Fmt.error_msg "drop: empty stack" )
      | Unreachable -> Fmt.error_msg "unreachable"
      | Block (_str_opt, bt, block_instrs) -> (
        let ty =
          match bt with
          | None -> { arg = []; result = [] }
          | Some (Bt_raw (_, ft)) -> func_type_to_bt ft
        in
        let l' = Some { form = Block; ty; code = [] } in
        let args = List.take (List.length ty.arg) sigma in
        let+ res =
          eval_instr ~no_input block_instrs.raw ((l', args, rho) :: states)
        in
        match res with
        | (_, sigma', rho') :: _ -> ((l, sigma' @ sigma, rho') :: states', instrs)
        | [] -> assert false )
      | Loop (_str_opt, bt, block_instrs) -> (
        let ty =
          match bt with
          | None -> { arg = []; result = [] }
          | Some (Bt_raw (_, ft)) -> func_type_to_bt ft
        in
        let l' = Some { form = Loop; ty; code = block_instrs.raw } in
        let args = List.take (List.length ty.arg) sigma in
        let+ res =
          eval_instr ~no_input block_instrs.raw ((l', args, rho) :: states)
        in
        match res with
        | (_, sigma', rho') :: _ -> ((l, sigma' @ sigma, rho') :: states', instrs)
        | [] -> assert false )
      | If_else (_str_opt, bt, then_instrs, else_instrs) ->
        let+ res =
          match sigma with
          | I32 0l :: sigma | I64 0L :: sigma ->
            eval_instr ~no_input
              [ Binary.Block (_str_opt, bt, then_instrs) |> Annotated.dummy ]
              ((l, sigma, rho) :: states')
          | _ ->
            eval_instr ~no_input
              [ Binary.Block (_str_opt, bt, else_instrs) |> Annotated.dummy ]
              ((l, sigma, rho) :: states')
        in
        (res, instrs)
      | Br id -> (
        match l with
        | None -> Fmt.error_msg "br: on root (should be typechecked)"
        | Some lbl when id = 0 -> (
          match lbl.form with
          | Block ->
            let results = List.take (List.length lbl.ty.result) sigma in
            Ok ((Some lbl, results, rho) :: states', instrs)
          | Loop ->
            let args = List.take (List.length lbl.ty.arg) sigma in
            Ok ((Some lbl, args, rho) :: states', instrs)
          | Func -> Fmt.error_msg "TODO: br func" )
        | Some lbl -> (
          match lbl.form with
          | Func ->
            Fmt.error_msg
              "br: trying to go higher than func (should be typechecked)"
          | _ -> (
            match states with
            | _ :: f -> Ok (f, i :: instrs)
            | [] -> Fmt.error_msg "br: reached the top (should be typechecked)"
            ) ) )
      | Br_if id -> (
        match sigma with
        | I32 0l :: sigma | I64 0L :: sigma ->
          let br = Annotated.dummy (Binary.Br id) in
          Ok ((l, sigma, rho) :: states', br :: instrs)
        | _ :: sigma -> Ok ((l, sigma, rho) :: states', instrs)
        | [] -> Fmt.error_msg "br_if: empty stack" )
      | Return -> (
        match states' with
        | (l', sigma', rho') :: state' ->
          Ok ((l', sigma @ sigma', rho') :: state', instrs)
        | [] ->
          assert (List.length instrs = 0);
          Ok ([], []) )
      | instr ->
        Fmt.error_msg "TODO implement instr %a@\n"
          (Binary.pp_instr ~short:false)
          instr
      end
    in
    if not no_input then input_loop res;
    eval_instr ~no_input res_instrs res
  end
  | [] ->
    print_state states;
    Ok states

let run ~no_input (m : Binary.Module.t Result.t) =
  let+ m in
  let start = m.func.(option_get m.start) in
  let start = match start with Local a -> a | _ -> assert false in
  let state = (None, [], Map.empty) :: [] in
  let res = eval_instr start.body.raw state ~no_input in
  match res with
  | Ok _state -> Fmt.pr "Ok@."
  | Error e -> Fmt.epr "%s@." (Result.err_to_string e)
