open Binary
open Abs_datastructures
module ADomain = Abs_value.ADomain
module Size = Abs_value.Size

module Int = struct
  include Int

  let to_int = Fun.id
end

module Locals = PatriciaTree.MakeMap (Int)

type value = Abs_value.t

type state =
  { ctx : ADomain.Context.t
  ; stack : value Stack.t
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
    ADomain.context_pretty state.ctx
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

  let init_domain_result =
    ADomain.Context.Result
      (true, ADomain.Context.empty_tuple (), fun _ctx out -> ([], out))

  module JumpKey = struct
    type t = int option

    let decr = Option.map Int.pred

    let to_int = Option.value ~default:(-1)
  end

  module JumpTarget = struct
    module Map = PatriciaTree.MakeMap (JumpKey)

    type 'a t = 'a Map.t

    let of_list = Map.of_list

    let to_list = Map.to_list

    let empty = Map.empty

    let find = Map.find_opt

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
  end

  let ( let> ) (opt, mapp) f =
    match opt with Some v -> f (v, mapp) | None -> (None, mapp)

  let serialize ~widens : state -> state -> (state, 'a) ADomain.Context.result =
   fun state_a state_b ->
    let gen_new_value ~widens a b state_a state_b
      (ADomain.Context.Result (inc, intup, cont)) f =
      if Abs_value.equal a b then None
      else
        let size = Abs_value.size_of a in
        (* inc : whether the new value is included in the old one
         * intup : symbolic repr of all variabls that will be created simultaneously
         * cont : continuation function
         *)
        let (ADomain.Context.Result (inc, intup, local_cont)) =
          ADomain.serialize_binary ~size ~widens state_a.ctx
            (Abs_value.to_binary a) state_b.ctx (Abs_value.to_binary b)
            (inc, intup)
        in
        let cont ctx out_tuple =
          let integer, out_tuple = local_cont ctx out_tuple in
          let list, out_tuple = cont ctx out_tuple in
          let b = Abs_value.of_binary size integer in
          (f b list, out_tuple)
        in
        Some (ADomain.Context.Result (inc, intup, cont))
    in
    let rec serliaze_stack lhs rhs acc_res =
      match (lhs, rhs) with
      | [], [] -> acc_res
      | [], _ :: _ | _ :: _, [] ->
        Fmt.failwith "join on stacks of different sizes"
      | v1 :: rest_a, v2 :: rest_b -> begin
        let r = gen_new_value ~widens v1 v2 state_a state_b acc_res List.cons in
        serliaze_stack rest_a rest_b
          (match r with Some res -> res | None -> acc_res)
      end
    in
    let (ADomain.Context.Result (included, in_tuple, locals_continue)) =
      Locals.fold_on_nonequal_union
        (fun k v1 v2 res ->
          let size =
            match (v1, v2) with
            | Some v, _ | _, Some v -> Abs_value.size_of v
            | _ ->
              (* at least one of the two has to be defined *)
              assert false
          in
          let v1 = Option.value v1 ~default:(Abs_value.top size state_a.ctx) in
          let v2 = Option.value v2 ~default:(Abs_value.top size state_b.ctx) in
          match
            gen_new_value ~widens v1 v2 state_a state_b res (Locals.add k)
          with
          | Some res -> res
          | None -> res )
        state_a.locals state_b.locals
        (ADomain.Context.Result
           ( true
           , ADomain.Context.empty_tuple ()
           , fun _ctx out -> (state_a.locals, out) ) )
    in

    let (ADomain.Context.Result (included, in_tuple, stack_continue)) =
      serliaze_stack
        (Stack.to_list state_a.stack)
        (Stack.to_list state_b.stack)
        (ADomain.Context.Result (included, in_tuple, fun _ctx out -> ([], out)))
      (* TODO can we use this ? (ADomain.Context.Result *)
      (*    (true, ADomain.Context.empty_tuple (), fun _ctx out -> ([], out)) ) *)
    in

    (* TODO can we use this ? (ADomain.Context.Result *)
    (*    (true, ADomain.Context.empty_tuple (), ) *)
    ADomain.Context.Result
      ( included
      , in_tuple
      , fun ctx out ->
          let stack, out = stack_continue ctx out in
          let locals, out = locals_continue ctx out in
          ( { state_a with ctx; stack = Stack.of_list @@ List.rev stack; locals }
          , out ) )

  let join state_a state_b =
    let (ADomain.Context.Result (_inc, in_tuple, continue)) =
      serialize ~widens:false state_a state_b
    in
    let ctx, out = ADomain.typed_nondet2 state_a.ctx state_b.ctx in_tuple in
    fst @@ continue ctx out

  let join_jump_tables :
       state list JumpTarget.t
    -> state list JumpTarget.t
    -> ('a, 'b) ADomain.Context.result =
   fun _a _b -> assert false
  (* Map.fold_on_nonequal_union *)
  (*   (fun k va vb (ADomain.Context.Result (inc, intup, cont)) -> *)
  (*     gen_new_value ~widens:true ) *)
  (*   a b *)
  (*   (ADomain.Context.Result *)
  (*      (true, ADomain.Context.empty_tuple (), fun _ctx out -> assert false) *)
  (*   ) *)

  let widen widening_id state_a state_b =
    let (ADomain.Context.Result (included, in_tuple, continue)) =
      serialize ~widens:true state_a state_b
    in
    let ctx, included, out =
      ADomain.widened_fixpoint_step ~widening_id ~previous:state_a.ctx
        ~next:state_b.ctx (included, in_tuple)
    in
    let state, _ = continue ctx out in
    ({ state with ctx }, included)

  let rec eval_expr :
    t -> instr Annotated.t list -> t option * state list JumpTarget.t =
   fun state expr ->
    List.fold_left
      (fun (current_state, current_mapping) (x : instr Annotated.t) ->
        match current_state with
        | Some current_state ->
          let new_state, new_mapping = eval_instr current_state x.raw in
          (new_state, JumpTarget.append current_mapping new_mapping)
        | None -> assert false )
      (Some state, JumpTarget.empty)
      expr

  and eval_func (state : state) (func : Func.t) =
    Log.info (fun m ->
      m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
    let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
    let args, caller_popped_stack =
      Stack.pop_n state.stack (List.length param_type)
    in
    let init_value v =
      let size = Abs_value.size_of v in
      let zero = ADomain.Binary_Forward.biconst ~size Z.zero state.ctx in
      match v with Abs_value.I32 _ -> Abs_value.I32 zero | I64 _ -> I64 zero
    in
    let fn_state =
      { state with
        stack = Stack.empty
      ; ctx = state.ctx
      ; func_rt = result_type
      ; locals =
          Locals.of_list
          @@ List.mapi (fun i v -> (i, init_value v))
          @@ Stack.to_list args
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
    Some
      { state with
        stack =
          Stack.append caller_popped_stack
          @@ Stack.take (List.length result_type) func_end_state.stack
      }

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
          let v = ADomain.binary_unknown ~size:Size.b32 state.ctx in
          let stack = Stack.push state.stack (I32 v) in
          (Some { state with stack }, JumpTarget.empty)
        | 1 ->
          (* assume that it's correctly typed to returning an i64 *)
          let v = ADomain.binary_unknown ~size:Size.b64 state.ctx in
          let stack = Stack.push state.stack (I64 v) in
          (Some { state with stack }, JumpTarget.empty)
        | _ ->
          Fmt.failwith "Some day we will have proper external function support"
        )
      end
    | Block (_str_opt, _bt, expr) ->
      let> res, mapping = eval_expr state expr.raw in
      let state = { state with stack = res.stack; locals = res.locals } in
      let new_state =
        match JumpTarget.find (Some 0) mapping with
        | Some (br_state :: br_states) -> List.fold_left join br_state br_states
        | None | Some [] -> state
      in
      let mapping =
        JumpTarget.remove (Some 0) mapping
        |> JumpTarget.to_list
        |> List.map (fun (k, v) -> (JumpKey.decr k, v))
        |> JumpTarget.of_list
      in
      (Some new_state, mapping)
    | If_else (_, bt, expr_true, expr_false) ->
      let _b, stack = Stack.pop state.stack in
      (* faire un eq 0 puis donner ça en tête des expression de chaque branche avec un assume *)
      (* quand assume renvoie none = bottom *)
      let state_true, _ =
        eval_instr { state with stack } (Block (None, bt, expr_true))
      in
      let state_false, _ =
        eval_instr { state with stack } (Block (None, bt, expr_false))
      in
      begin match (state_true, state_false) with
      | Some state_true, Some state_false ->
        (Some (join state_true state_false), JumpTarget.empty)
      | _ -> (*TODO*) assert false
      end
    | Loop (_str_opt, _bt, body) ->
      let widening_id = Domains.Sig.Widening_Id.fresh () in
      let one_iteration state =
        (* update the state by one loop iteration: assume the condition and apply the body *)
        let i, stack = Stack.pop state.stack in
        let ctx = ADomain.assume state.ctx (Abs_value.to_boolean state.ctx i) in
        match ctx with
        | Some ctx -> eval_expr { state with ctx; stack } body.raw
        | None -> (None, JumpTarget.empty)
      in
      let initial_state = { state with ctx = ADomain.Context.copy state.ctx } in
      let rec loop state jt =
        let next_state, next_jt = one_iteration state in
        let next_state =
          match next_state with
          | Some next_head -> join initial_state next_head
          | None -> initial_state
        in
        let next_jt = join_jump_tables jt next_jt in
        let widened, included = widen widening_id state next_state in
        if not included then loop widened next_jt
        else (* fixpoint reached: exit loop, assume condition is false *)
          let cond, stack = Stack.pop state.stack in
          let ctx =
            Abs_value.to_boolean next_state.ctx cond
            |> ADomain.Boolean_Forward.not next_state.ctx
            |> ADomain.assume next_state.ctx
          in
          match ctx with
          | Some ctx -> (Some { next_state with ctx; stack }, next_jt)
          | None -> (None, next_jt)
      in
      loop state JumpTarget.empty
    | Br i -> (None, JumpTarget.of_list [ (Some i, [ state ]) ])
    | instr -> (
      let res = S.eval_instr state instr in
      match res with
      | Some s, None -> (Some s, JumpTarget.empty)
      | None, Some instr -> eval_instr state instr
      | Some _, Some _ -> (* should not happen *) assert false
      | None, None -> (None, JumpTarget.empty) )
end

(*===========================================================================*)

module DataState : DATA_STATE = struct
  type t = state option * instr option

  let rec exec_ibinop state size (op : Text.ibinop) : t =
    (* TODO: vérifier les overflows *)
    let e1, e2, stack = Stack.pop_2 state.stack in
    match op with
    | Add ->
      let flags = Flags.Biadd.no_overflow in
      let e =
        Abs_value.binop size
          (ADomain.Binary_Forward.biadd ~flags ~size state.ctx)
          e1 e2
      in
      (Some { state with stack = Stack.push stack e }, None)
    | Sub ->
      let flags = Operator.Flags.Bisub.no_overflow in
      let e =
        Abs_value.binop size
          (ADomain.Binary_Forward.bisub ~flags ~size state.ctx)
          e1 e2
      in
      (Some { state with stack = Stack.push stack e }, None)
    | Mul ->
      let flags = Flags.Bimul.pack ~nsw:true ~nuw:true in
      let e =
        Abs_value.binop size
          (ADomain.Binary_Forward.bimul ~flags ~size state.ctx)
          e1 e2
      in
      (Some { state with stack = Stack.push stack e }, None)
    | Div sx ->
      (* checker division par zéro *)
      let op =
        match sx with
        | S -> ADomain.Binary_Forward.bisdiv ~size state.ctx
        | U -> ADomain.Binary_Forward.biudiv ~size state.ctx
      in
      let e = Abs_value.binop size op e1 e2 in
      (Some { state with stack = Stack.push stack e }, None)
    | _ -> assert false

  and eval_instr : state -> instr -> t =
   fun state instr ->
    match instr with
    | I32_const i ->
      let abs_i =
        ADomain.Binary_Forward.biconst ~size:Size.b32 (Z.of_int32 i) state.ctx
      in
      (Some { state with stack = Stack.push state.stack (I32 abs_i) }, None)
    | I64_const i ->
      let abs_i =
        ADomain.Binary_Forward.biconst ~size:Size.b64 (Z.of_int64 i) state.ctx
      in
      (Some { state with stack = Stack.push state.stack (I64 abs_i) }, None)
    | Unreachable ->
      (*TODO à gèrer proprement*)
      (None, None)
    | I_binop (nn, op) ->
      let size = match nn with Text.S32 -> Size.b32 | Text.S64 -> Size.b64 in
      exec_ibinop state size op
    | Local_get i -> (
      let var = Locals.find_opt i state.locals in
      match var with
      | Some v -> (Some { state with stack = Stack.push state.stack v }, None)
      | None ->
        Log.debug (fun m -> m "local.get on unset i:%i" i);
        assert false )
    | Local_set i ->
      let e, stack = Stack.pop state.stack in
      (Some { state with stack; locals = Locals.add i e state.locals }, None)
    | Drop ->
      let _, stack = Stack.pop state.stack in
      (Some { state with stack }, None)
    | Nop ->
      let top = ADomain.binary_unknown ~size:Size.b32 state.ctx in
      (Some { state with stack = Stack.push state.stack (I32 top) }, None)
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
  let ctx = ADomain.root_context () in
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
