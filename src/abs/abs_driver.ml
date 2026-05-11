open Binary
module Stack = Abs_stack.Make (Abs_value)
module D = Abs_value.ADomain
module Size = Abs_value.Size

module Int = struct
  include Int

  let to_int = Fun.id
end

module Locals = PatriciaTree.MakeMap (Int)

type value = Abs_value.t

type state =
  { ctx : D.Context.t
  ; stack : Stack.t
  ; locals : value Locals.t
  ; func_rt : val_type list
  ; env : Abs_extern_func.extern_func Link_env.t
  ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
  }

let ( let* ) = Option.bind

module Flags = Operator.Flags

let pp_state : Format.formatter -> state -> unit =
 fun fmt state ->
  Fmt.pf fmt "{@\n@[<hov 2>  ctx : %a,@;stack : %a,@;locals : %a@]@\n}"
    D.context_pretty state.ctx
    (Stack.pp @@ Abs_value.pp state.ctx)
    state.stack
    (Fmt.list ~sep:Fmt.semi (Abs_value.pp state.ctx))
    (Locals.to_list state.locals |> List.map snd)

module type DATA_STATE = sig
  type t = state option * instr option

  val eval_instr : state -> instr -> t
end

(*===========================================================================*)

module DenotFixpoint (S : DATA_STATE) = struct
  type t = state

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

    let append (old : 'a list t) (neww : 'a list t) =
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

  let ( let> ) (opt, mapp) f =
    match opt with Some v -> f (v, mapp) | None -> (None, mapp)

  let serialize ~widens : state -> state -> (state, 'a) D.Context.result =
   fun state_a state_b ->
    let gen_new_value ~widens a b state_a state_b
      (D.Context.Result (inc, intup, cont)) f =
      if Abs_value.equal a b then None
      else
        let size = Abs_value.size_of a in
        (* inc : whether the new value is included in the old one
         * intup : symbolic repr of all variabls that will be created simultaneously
         * cont : continuation function
         *)
        let (D.Context.Result (inc, in_tup, local_cont)) =
          D.serialize_binary ~size ~widens state_a.ctx (Abs_value.to_binary a)
            state_b.ctx (Abs_value.to_binary b) (inc, intup)
        in
        let cont ctx out_tuple =
          let integer, out_tuple = local_cont ctx out_tuple in
          let list, out_tuple = cont ctx out_tuple in
          let b = Abs_value.of_binary size integer in
          (f b list, out_tuple)
        in
        Some (D.Context.Result (inc, in_tup, cont))
    in
    let rec serialize_stack lhs rhs acc_res =
      match (lhs, rhs) with
      | [], [] -> acc_res
      | [], _ :: _ | _ :: _, [] ->
        Fmt.failwith "join on stacks of different sizes"
      | v1 :: rest_a, v2 :: rest_b -> begin
        let r = gen_new_value ~widens v1 v2 state_a state_b acc_res List.cons in
        serialize_stack rest_a rest_b
          (match r with Some res -> res | None -> acc_res)
        end
    in
    let (D.Context.Result (included, in_tuple, locals_continue)) =
      Locals.fold_on_nonequal_union
        begin fun k v1 v2 res ->
          let size =
            (* v1 and v2 should have the same size *)
            match v1 with
            | Some v -> Abs_value.size_of v
            | None -> assert false
          in
          let v1 = Option.value v1 ~default:(Abs_value.top size state_a.ctx) in
          let v2 = Option.value v2 ~default:(Abs_value.top size state_b.ctx) in
          let f = Locals.add k in
          match gen_new_value ~widens v1 v2 state_a state_b res f with
          | Some res -> res
          | None -> res
        end
        state_a.locals state_b.locals
        (D.Context.Result
           ( true
           , D.Context.empty_tuple ()
           , fun _ctx out -> (state_a.locals, out) ) )
    in
    let (D.Context.Result (inc, in_tup, stack_continue)) =
      serialize_stack
        (Stack.to_list state_a.stack)
        (Stack.to_list state_b.stack)
        (D.Context.Result (included, in_tuple, fun _ctx out -> ([], out)))
    in
    let cont ctx out =
      let stack, out = stack_continue ctx out in
      let locals, out = locals_continue ctx out in
      ( { state_a with ctx; stack = Stack.of_list @@ List.rev stack; locals }
      , out )
    in
    D.Context.Result (inc, in_tup, cont)

  let join state_a state_b =
    let (D.Context.Result (_inc, in_tuple, continue)) =
      serialize ~widens:false state_a state_b
    in
    let ctx, out = D.typed_nondet2 state_a.ctx state_b.ctx in_tuple in
    fst @@ continue ctx out

  let widen widening_id state_a state_b =
    let (D.Context.Result (included, in_tuple, continue)) =
      serialize ~widens:true state_a state_b
    in
    let ctx, included, out =
      D.widened_fixpoint_step ~widening_id ~previous:state_a.ctx
        ~next:state_b.ctx (included, in_tuple)
    in
    let state, _ = continue ctx out in
    ({ state with ctx }, included)

  let rec eval_expr :
    t -> instr Annotated.t list -> t option * state list JumpTarget.t =
   fun state expr ->
    let rec loop (state, jt) (expr : instr Annotated.t list) =
      match expr with
      | [] -> (Some state, jt)
      | instr :: instrs -> (
        let new_state, new_jt = eval_instr state instr.raw in
        let new_jt = JumpTarget.append jt new_jt in
        match new_state with
        | None -> (None, new_jt)
        | Some s -> loop (s, new_jt) instrs )
    in
    loop (state, JumpTarget.empty) expr

  and eval_func (state : state) (func : Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
    let args, caller_popped_stack =
      Stack.pop_n state.stack (List.length param_type)
    in
    let init_value (vt : val_type) =
      let size =
        match vt with
        | Num_type t -> (
          match t with I32 -> Size.b32 | I64 -> Size.b64 | _ -> assert false )
        | Ref_type _ -> assert false
      in
      let zero = D.Binary_Forward.biconst ~size Z.zero state.ctx in
      match vt with
      | Num_type I32 -> Abs_value.I32 zero
      | Num_type I64 -> I64 zero
      | _ -> assert false
    in
    let locals =
      List.map (fun (_str_opt, vt) -> init_value vt) func.locals
      |> List.mapi (fun i v -> (i, v))
      |> Locals.of_list
    in
    let fn_state =
      { state with
        stack = args
      ; ctx = state.ctx
      ; func_rt = result_type
      ; locals
      }
    in
    Log.info (fun m -> m "Func start state : %a" pp_state fn_state);
    (* TODO: handle mapping *)
    let func_end_state, _ = eval_expr fn_state func.body.raw in
    Log.info (fun m ->
      m "Func end state : %a@."
        (Fmt.option ~none:(Fmt.any "None") pp_state)
        func_end_state );
    (* We should probably copy state and join back the return values in the context here *)
    let* func_end_state in
    let stack =
      Stack.append caller_popped_stack
      @@ Stack.take (List.length result_type) func_end_state.stack
    in
    Some { state with stack }

  and eval_instr : t -> instr -> t option * state list JumpTarget.t =
   fun state instr ->
    Log.debug (fun m ->
      m "#%a\t\t%a@." (pp_instr ~short:true) instr pp_state state );
    match instr with
    | Call idx ->
      let func = Link_env.get_func state.env idx in
      begin match func with
      | Wasm { func; idx } ->
        let env = Dynarray.get state.envs idx in
        let r = eval_func { state with env } func in
        (r, JumpTarget.empty)
      | Extern { idx } -> (
        match idx with
        | 0 ->
          (* assume that it's correctly typed to returning an i32 *)
          let v = D.binary_unknown ~size:Size.b32 state.ctx in
          let stack = Stack.push state.stack (I32 v) in
          (Some { state with stack }, JumpTarget.empty)
        | 1 ->
          (* assume that it's correctly typed to returning an i64 *)
          let v = D.binary_unknown ~size:Size.b64 state.ctx in
          let stack = Stack.push state.stack (I64 v) in
          (Some { state with stack }, JumpTarget.empty)
        | _ ->
          Fmt.failwith "Some day we will have proper external function support"
        )
      end
    | Block (_str_opt, _bt, expr) ->
      (* TODO il faut gérer le mapping pour le cas de None *)
      let> res, mapping = eval_expr state expr.raw in
      let state = { state with stack = res.stack; locals = res.locals } in
      let new_state =
        match JumpTarget.find_opt (I 0) mapping with
        | Some br_states -> List.fold_left join state br_states
        | None -> state
      in
      let mapping =
        (* TODO on peut avoir une paire de (int * map) pour ne pas avoir à decr la liste immédiatement *)
        JumpTarget.decr mapping
      in
      (Some new_state, mapping)
    | If_else (_, bt, expr_then, expr_else) ->
      let b, stack = Stack.pop state.stack in
      let cond = Abs_value.to_boolean state.ctx b in
      let state_then, jt_true =
        let> ctx, _ = (D.assume state.ctx cond, JumpTarget.empty) in
        eval_instr { state with stack; ctx } (Block (None, bt, expr_then))
      in
      let state_else, jt_false =
        let not_cond = D.Boolean_Forward.not state.ctx cond in
        let> ctx, _ = (D.assume state.ctx not_cond, JumpTarget.empty) in
        eval_instr { state with stack; ctx } (Block (None, bt, expr_else))
      in
      let jt = JumpTarget.append jt_true jt_false in
      begin match (state_then, state_else) with
      | Some state_true, Some state_false ->
        (Some (join state_true state_false), jt)
      | Some state, None | None, Some state -> (Some state, jt)
      | None, None ->
        (* TODO should this be assert false ? *)
        (None, jt)
      end
    | Loop (_str_opt, bt, body) ->
      let widening_id = Domains.Sig.Widening_Id.fresh () in
      (* TODO tester si on a besoin de copie *)
      let initial_state = { state with ctx = D.Context.copy state.ctx } in
      let rec fixpoint state =
        let next_state, jt = eval_expr state body.raw in
        let to_take =
          match bt with
          | Some (Bt_raw (_i, (params, _res))) -> List.length params
          | None -> 0
        in
        let shorten_stack stack = Stack.take to_take stack in
        let next_head =
          match JumpTarget.find_opt (I 0) jt with
          | Some jts ->
            let fp_stack = shorten_stack initial_state.stack in
            List.fold_left
              (fun acc state ->
                let stack = shorten_stack state.stack in
                join acc { state with stack } )
              { initial_state with stack = fp_stack }
              jts
          | None -> assert false
        in
        let widened, included = widen widening_id state next_head in
        if not included then fixpoint widened
        else
          (* fixpoint reached: exit loop, assume condition is false *)
          let jt = JumpTarget.decr jt in
          let next_state =
            Option.bind next_state @@ fun next_state ->
            let stack = Stack.append next_state.stack initial_state.stack in
            Some { next_state with stack }
          in
          (next_state, jt)
      in
      fixpoint state
    | Br i -> (None, JumpTarget.of_list [ (I i, [ state ]) ])
    | instr -> (
      let res = S.eval_instr state instr in
      match res with
      | Some s, None -> (Some s, JumpTarget.empty)
      | None, Some instr -> eval_instr state instr
      | Some _, Some _ -> (* should not happen *) assert false
      | None, None -> (* unreachable *) (None, JumpTarget.empty) )
end

(*===========================================================================*)

module DataState : DATA_STATE = struct
  (*TODO on peut utiliser une exception*)
  type t = state option * instr option

  module Binop = struct
    (* TODO vérifier les overflows *)
    let binop stack size op =
      let e1, e2, stack = Stack.pop_2 stack in
      let lhs, rhs = (Abs_value.to_binary e1, Abs_value.to_binary e2) in
      let bin_res = op lhs rhs in
      let r = Abs_value.of_binary size bin_res in
      let stack = Stack.push stack r in
      stack

    let add state size =
      let flags = Flags.Biadd.no_overflow in
      let op = D.Binary_Forward.biadd ~flags ~size state.ctx in
      let stack = binop state.stack size op in
      { state with stack }

    let sub state size =
      let flags = Operator.Flags.Bisub.no_overflow in
      let op = D.Binary_Forward.bisub ~flags ~size state.ctx in
      let stack = binop state.stack size op in
      { state with stack }

    let mul state size =
      let flags = Flags.Bimul.pack ~nsw:true ~nuw:true in
      let op = D.Binary_Forward.bimul ~flags ~size state.ctx in
      let stack = binop state.stack size op in
      { state with stack }

    let div state (sx : Text.sx) size =
      let op =
        match sx with
        | S -> D.Binary_Forward.bisdiv ~size state.ctx
        | U -> D.Binary_Forward.biudiv ~size state.ctx
      in
      let stack = binop state.stack size op in
      { state with stack }
  end

  module Relop = struct
    let relop ?(not = None) state size op =
      let e1, e2, stack = Stack.pop_2 state.stack in
      let lhs, rhs = (Abs_value.to_binary e1, Abs_value.to_binary e2) in
      let bool_res = op lhs rhs in
      let bool_res =
        match not with
        | Some v ->
          D.Boolean_Forward.( && ) state.ctx
            (D.Boolean_Forward.not state.ctx v)
            bool_res
        | None -> bool_res
      in
      let r = Abs_value.of_boolean state.ctx size bool_res in
      let stack = Stack.push stack r in
      stack

    let _and state size =
      let op = D.Binary_Forward.band ~size state.ctx in
      let stack = Binop.binop state.stack size op in
      { state with stack }

    let _or state size =
      let op = D.Binary_Forward.bor ~size state.ctx in
      let stack = Binop.binop state.stack size op in
      { state with stack }

    let le state (sx : Text.sx) size =
      let op =
        match sx with
        | S -> D.Binary_Forward.bisle ~size state.ctx
        | U -> D.Binary_Forward.biule ~size state.ctx
      in
      let stack = relop state size op in
      { state with stack }

    let lt state (sx : Text.sx) size =
      let op =
        match sx with
        | S -> D.Binary_Forward.bisle ~size state.ctx
        | U -> D.Binary_Forward.biule ~size state.ctx
      in
      let stack = relop state size op in
      { state with stack }
  end

  let eval_i32 state (instr : Binary.i32_instr) =
    let size = Size.b32 in
    match instr with
    | Const i ->
      let abs_i = D.Binary_Forward.biconst ~size (Z.of_int32 i) state.ctx in
      let stack = Stack.push state.stack (Abs_value.I32 abs_i) in
      { state with stack }
    | Add -> Binop.add state size
    | Sub -> Binop.sub state size
    | Mul -> Binop.mul state size
    | Div sx -> Binop.div state sx size
    | And -> Relop._and state size
    | Or -> Relop._or state size
    | Lt sx -> Relop.lt state sx size
    | Le sx -> Relop.le state sx size
    | _ ->
      Fmt.epr "not implemented yet";
      assert false

  let eval_i64 state (instr : Binary.i64_instr) =
    let size = Size.b64 in
    match instr with
    | Const i ->
      let abs_i =
        D.Binary_Forward.biconst ~size:Size.b64 (Z.of_int64 i) state.ctx
      in
      let stack = Stack.push state.stack (Abs_value.I32 abs_i) in
      { state with stack }
    | Add -> Binop.add state size
    | Sub -> Binop.sub state size
    | Mul -> Binop.mul state size
    | Div sx -> Binop.div state sx size
    | _ -> assert false

  let eval_local state : Binary.local_instr -> _ = function
    | Get i ->
      let v = Locals.find i state.locals in
      let stack = Stack.push state.stack v in
      { state with stack }
    | Set i ->
      let e, stack = Stack.pop state.stack in
      let locals = Locals.add i e state.locals in
      { state with stack; locals }
    | Tee i ->
      let e, stack = Stack.pop state.stack in
      let stack = Stack.push stack e in
      let locals = Locals.add i e state.locals in
      { state with stack; locals }

  let eval_instr : state -> instr -> t =
   fun state instr ->
    match instr with
    | I32 instr ->
      let r = eval_i32 state instr in
      (Some r, None)
    | I64 instr ->
      let r = eval_i64 state instr in
      (Some r, None)
    | Unreachable ->
      (*TODO à gèrer proprement*)
      (None, None)
    | Local instr ->
      let state = eval_local state instr in
      (Some state, None)
    | Drop ->
      let _, stack = Stack.pop state.stack in
      (Some { state with stack }, None)
    | Nop -> (Some state, None)
    | ( If_else _ | Call _ | Block _ | Loop _ | Br _ | Br_if _ | Br_table _
      | Br_on_non_null _ | Br_on_null _ ) as instr ->
      (Some state, Some instr)
    | instr ->
      Fmt.failwith "DataState.eval_instr not implemented for %a"
        (pp_instr ~short:true) instr
end

module ConcreteFixpoint = DenotFixpoint (DataState)

let expr (link_state : Abs_extern_func.extern_func Link.State.t)
  (m : Abs_extern_func.extern_func Linked.Module.t) =
  let envs = Link.State.get_envs link_state in
  let ctx = D.root_context () in
  let initial_state =
    { ctx
    ; stack = Stack.empty
    ; locals = Locals.empty
    ; env = m.env
    ; func_rt = []
    ; envs
    }
  in

  List.iter
    begin fun (e : expr Annotated.t) ->
      let end_state, _ = ConcreteFixpoint.eval_expr initial_state e.raw in
      Fmt.pr "End State : %a@."
        (Fmt.option ~none:(Fmt.any "none") pp_state)
        end_state
    end
    m.to_run
