open Syntax

module Int = struct
  include Int

  let to_int = Fun.id
end

module M (State : sig
  type t

  val pp : t Fmt.t

  type ctx

  val pp_ctx : t Fmt.t

  module Locals : sig end

  module Stack : sig end
end) (Value : sig
  type i32

  type i64

  module I32 : sig
    type t := i32

    val of_concrete : ?ctx:State.ctx -> Concrete_i32.t -> t

    val eqz : ?ctx:State.ctx -> t -> bool

    val add : ?ctx:State.ctx -> t -> t -> t

    val sub : ?ctx:State.ctx -> t -> t -> t

    val to_int : ?ctx:State.ctx -> t -> int
  end

  module I64 : sig
    type t := i64

    val of_concrete : ?ctx:State.ctx -> Concrete_i64.t -> t

    val eqz : ?ctx:State.ctx -> t -> bool

    val add : ?ctx:State.ctx -> t -> t -> t

    val sub : ?ctx:State.ctx -> t -> t -> t

    val mul : ?ctx:State.ctx -> t -> t -> t
  end

  type t =
    | I32 of i32
    | I64 of i64

  val pp : State.t -> t Fmt.t
end) =
struct
  module Stack = Abs_stack.Make (struct
    type i32 = Value.i32

    type i64 = Value.i64

    type t = Value.t =
      | I32 of i32
      | I64 of i64
  end)

  type sigma = Stack.t

  type bf =
    | Block
    | Loop
    | Func

  type bt =
    { arg : Binary.val_type list
    ; result : Binary.val_type list
    }

  type continuation =
    { form : bf
    ; ty : bt
    ; code : Binary.expr
    }

  module Locals = PatriciaTree.MakeMap (Int)

  type e = Value.t Locals.t

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

  let pp_l fmt { form; ty; code } =
    Fmt.pf fmt "(%s (%a)->(%a) %s)"
      (match form with Block -> "b" | Loop -> "l" | Func -> "f")
      (Fmt.list ~sep:(Fmt.any ", ") Binary.pp_val_type)
      ty.arg
      (Fmt.list ~sep:(Fmt.any ", ") Binary.pp_val_type)
      ty.result
      (if List.length code > 0 then "I" else "[]")

  let pp_map =
    Locals.pretty (fun fmt k v -> Fmt.pf fmt "%i->%a" k (Value.pp ()) v)

  let print_state (state : state) =
    let sigma, rho = state in
    Fmt.pr "@[<hov>σ:[%a];@;ρ:%a@]@." (Stack.pp @@ Value.pp ()) sigma pp_map rho

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
    let params = List.map snd params in
    { arg = params; result = results }

  (*=========================================================================*)

  let i32_binop stack op =
    let v1, v2, stack = Stack.pop_2 stack in
    match (v1, v2) with
    | Value.I32 i1, I32 i2 -> Stack.push stack (I32 (op i1 i2))
    | _ -> assert false

  let eval_i32 (sigma, rho) : Binary.i32_instr -> _ = function
    | Binary.Const i ->
      let sigma = Stack.push sigma (I32 (Value.I32.of_concrete i)) in
      (sigma, rho)
    | Add ->
      let sigma = i32_binop sigma Value.I32.add in
      (sigma, rho)
    | Sub ->
      let sigma = i32_binop sigma Value.I32.sub in
      (sigma, rho)
    | _ -> assert false

  let i64_binop stack op =
    let v1, v2, stack = Stack.pop_2 stack in
    match (v1, v2) with
    | I64 i1, I64 i2 -> Stack.push stack (I64 (op i1 i2))
    | _ -> assert false

  let eval_i64 (sigma, rho) : Binary.i64_instr -> _ = function
    | Binary.Const i ->
      let sigma = Stack.push sigma (I64 (Value.I64.of_concrete i)) in
      (sigma, rho)
    | Add ->
      let sigma = i64_binop sigma Value.I64.add in
      (sigma, rho)
    | Sub ->
      let sigma = i64_binop sigma Value.I64.sub in
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
    state -> Binary.instr Annotated.t list -> state option * state JumpTarget.t
      =
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
      | Value.I32 i when Value.I32.eqz i ->
        eval_instr (sigma, rho) (Block (str_opt, bt, expr_else))
      | _ -> eval_instr (sigma, rho) (Block (str_opt, bt, expr_then)) )
    | Br i -> (None, JumpTarget.of_list [ (I i, state) ])
    | Br_if id -> (
      let v, sigma = Stack.pop sigma in
      match v with
      | I32 i when Value.I32.eqz i -> (Some (sigma, rho), JumpTarget.empty)
      | _ ->
        (* br i *)
        (None, JumpTarget.of_list [ (I id, state) ]) )
    | Br_table (cases, default) ->
      let v, sigma = Stack.pop sigma in
      let v_int =
        match v with I32 i -> Value.I32.to_int i | _ -> assert false
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
end

module Context = struct
  type t = unit

  let pp (_ : Format.formatter) () = ()
end

module Value = struct
  type i32 = Concrete_i32.t

  type i64 = Concrete_i64.t

  type t =
    | I32 of i32
    | I64 of i64

  module I32 = struct
    include Concrete_i32

    let eqz i = eq zero i
  end

  module I64 = struct
    include Concrete_i64

    let eqz i = eq zero i
  end

  let pp () fmt = function
    | I32 i -> Fmt.pf fmt "%ld" i
    | I64 i -> Fmt.pf fmt "%Ld" i
end

include M (Context) (Value)
