open Syntax

module Int = struct
  include Int

  let to_int = Fun.id
end

type t =
  | I32
  | I64

type v =
  | I32 of int32
  | I64 of int64

module Stack = Abs_stack.Make (struct
  type t = v
end)

type sigma = Stack.t

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

module Locals = PatriciaTree.MakeMap (Int)

type e = v Locals.t

type state = sigma * e

module JumpKey = struct
  (* TODO c'est mieux de définir le type nous même *)
  type t =
    | I of int
    | Ret

  let map : (int -> int) -> t -> t =
   fun f key -> match key with I i -> I (f i) | Ret -> Ret

  let decr = map Int.pred

  let to_int = function I i -> i | Ret -> -1
end

module JumpTarget = struct
  module Map = PatriciaTree.MakeMap (JumpKey)

  type 'a t = 'a Map.t

  let of_list = Map.of_list

  let to_list = Map.to_list

  let empty = Map.empty

  let find_opt = Map.find_opt

  let remove = Map.remove

  let add m k state =
    match Map.find_opt k m with
    | Some state_list -> Map.add k (state :: state_list) m
    | None -> Map.add k [ state ] m

  let append (old : 'a t) (neww : 'a t) =
    Map.mapi
      (fun k el ->
        match Map.find_opt k old with
        | Some el' -> List.append el el'
        | None -> el )
      neww

  let decr jt =
    remove (I 0) jt |> to_list
    |> List.map (fun (k, v) -> (JumpKey.decr k, v))
    |> of_list
end

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

let pp_map = Locals.pretty (fun fmt k v -> Fmt.pf fmt "%i->%a" k pp_v v)

let print_state (state : state) =
  let sigma, rho = state in
  Fmt.pr "@[<hov>σ:[%a];@;ρ:%a@]@." (Stack.pp pp_v) sigma pp_map rho

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

let option_get = function Some x -> x | None -> assert false [@@inline]

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

let i32_binop stack op =
  let v1, v2, stack = Stack.pop_2 stack in
  match (v1, v2) with
  | I32 i1, I32 i2 -> Stack.push stack (I32 (op i1 i2))
  | _ -> assert false

let eval_i32 (sigma, rho) : Binary.i32_instr -> _ = function
  | Binary.Const i ->
    let sigma = Stack.push sigma (I32 i) in
    (sigma, rho)
  | Add ->
    let sigma = i32_binop sigma Int32.add in
    (sigma, rho)
  | Sub ->
    let sigma = i32_binop sigma Int32.sub in
    (sigma, rho)
  | _ -> assert false

let i64_binop stack op =
  let v1, v2, stack = Stack.pop_2 stack in
  match (v1, v2) with
  | I64 i1, I64 i2 -> Stack.push stack (I64 (op i1 i2))
  | _ -> assert false

let eval_i64 (sigma, rho) : Binary.i64_instr -> _ = function
  | Binary.Const i ->
    let sigma = Stack.push sigma (I64 i) in
    (sigma, rho)
  | Add ->
    let sigma = i64_binop sigma Int64.add in
    (sigma, rho)
  | Sub ->
    let sigma = i64_binop sigma Int64.sub in
    (sigma, rho)
  | _ -> assert false

let eval_local (sigma, rho) : Binary.local_instr -> _ = function
  | Get i -> (
    match Locals.find_opt i rho with
    | None -> Fmt.failwith "local.get: local %i is not set" i
    | Some v -> (v :: sigma, rho) )
  | Set i ->
    let v, sigma = Stack.pop sigma in
    let rho = Locals.add i v rho in
    (sigma, rho)
  | Tee i ->
    let v, sigma = Stack.pop sigma in
    let rho = Locals.add i v rho in
    let sigma = Stack.push sigma v in
    (sigma, rho)

let join lhs rhs = match rhs with Some x -> Some x | None -> lhs

let rec eval_expr :
  state -> Binary.instr Annotated.t list -> state option * state JumpTarget.t =
 fun state expr ->
  let rec loop (state, jt) (expr : Binary.instr Annotated.t list) =
    match expr with
    | [] -> (Some state, jt)
    | instr :: instrs -> (
      let new_state, new_jt = eval_instr state instr.raw in
      let new_jt =
        JumpTarget.Map.idempotent_union (fun _ -> assert false) jt new_jt
      in
      match new_state with
      | None -> (None, new_jt)
      | Some s -> loop (s, new_jt) instrs )
  in
  loop (state, JumpTarget.empty) expr

and eval_instr : state -> Binary.instr -> state option * state JumpTarget.t =
 fun state instr ->
  let sigma, rho = state in
  match instr with
  | Binary.I32 instr -> (Some (eval_i32 state instr), JumpTarget.empty)
  | Binary.I64 instr -> (Some (eval_i64 state instr), JumpTarget.empty)
  | Drop ->
    let _, sigma = Stack.pop sigma in
    (Some (sigma, rho), JumpTarget.empty)
  | Unreachable -> (None, JumpTarget.empty)
  | Block (_str_opt, _bt, body) ->
    let res, mapping = eval_expr state body.raw in
    let m_decr = JumpTarget.decr mapping in
    let s = join res (JumpTarget.find_opt (I 0) mapping) in
    (s, m_decr)
  | Loop (_str_opt, _bt, _block_instrs) -> assert false
  | If_else (str_opt, bt, expr_then, expr_else) -> (
    let v, sigma = Stack.pop sigma in
    match v with
    | I32 0l -> eval_instr (sigma, rho) (Block (str_opt, bt, expr_else))
    | _ -> eval_instr (sigma, rho) (Block (str_opt, bt, expr_then)) )
  | Br i -> (None, JumpTarget.of_list [ (I i, state) ])
  | Br_if id -> (
    let v, sigma = Stack.pop sigma in
    match v with
    | I32 0l -> (Some (sigma, rho), JumpTarget.empty)
    | _ ->
      (* br i *)
      (None, JumpTarget.of_list [ (I id, state) ]) )
  | Br_table (cases, default) ->
    let v, sigma = Stack.pop sigma in
    let v_int =
      match v with
      (* br_table works only on int32 *)
      | I32 i -> Int32.to_int i
      | _ -> assert false
    in
    let matched = Array.find_index (fun case -> case = v_int) cases in
    let jt = match matched with Some i -> i | None -> default in
    (None, JumpTarget.of_list [ (I jt, (sigma, rho)) ])
  | Return -> (None, JumpTarget.of_list [ (Ret, state) ])
  | instr ->
    Fmt.failwith "TODO implement instr %a@\n"
      (Binary.pp_instr ~short:false)
      instr

let run (m : Binary.Module.t Result.t) =
  let+ m in
  let start = m.func.(option_get m.start) in
  let start = match start with Local a -> a | _ -> assert false in
  let state = (Stack.empty, Locals.empty) in
  let res, _ = eval_expr state start.body.raw in
  match res with Some _state -> Fmt.pr "Ok@." | None -> Fmt.pr "None@."
