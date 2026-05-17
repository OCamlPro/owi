open Binary

module Int = struct
  include Int

  let to_int = Fun.id
end

module M (Value0 : sig
  type i32

  type i64

  type boolean

  type t =
    | I32 of i32
    | I64 of i64
end) (State : sig
  type context

  module Locals : sig
    include PatriciaTree.MAP with type key = Int.t
  end

  module Stack :
    Abs_stack.S
      with type value := Value0.t
       and type i32 := Value0.i32
       and type i64 := Value0.i64

  type t =
    { ctx : context
    ; stack : Stack.t
    ; locals : Value0.t Locals.t
    ; func_rt : result_type
    ; env : Abs_extern_func.extern_func Link_env.t (* todo *)
    ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
    }

  val join : t -> t -> t

  val widen : Codex.Domains.Sig.Widening_Id.t -> t -> t -> t * bool

  val assume : t -> Value0.boolean -> t option

  val pp : t Fmt.t

  val pp_ctx : t Fmt.t
end) (Value : sig
  module Boolean : sig
    type t := Value0.boolean

    val of_i32 : ?state:State.t -> Value0.i32 -> t

    val to_i32 : ?state:State.t -> t -> Value0.i32

    val _true : Value0.boolean

    val _false : Value0.boolean

    val not : ?state:State.t -> Value0.boolean -> Value0.boolean
  end

  module I32 : sig
    type t := Value0.i32

    val zero : ?state:State.t -> t

    val of_concrete : ?state:State.t -> Concrete_i32.t -> t

    val eqz : ?state:State.t -> t -> bool

    val add : ?state:State.t -> t -> t -> t

    val sub : ?state:State.t -> t -> t -> t

    val mul : ?state:State.t -> t -> t -> t

    val div : Text.sx -> ?state:State.t -> t -> t -> t

    val to_int : t -> int

    val logand : ?state:State.t -> t -> t -> t

    val logor : ?state:State.t -> t -> t -> t

    (* val lt : ?state:State.t -> t -> t -> boolean *)
    (**)
    (* val le : ?state:State.t -> t -> t -> boolean *)
    (**)
    (* val gt : ?state:State.t -> t -> t -> boolean *)
    (**)
    (* val ge : ?state:State.t -> t -> t -> boolean *)
  end

  module I64 : sig
    type t := Value0.i64

    val zero : ?state:State.t -> t

    val of_concrete : ?state:State.t -> Concrete_i64.t -> t

    val eqz : ?state:State.t -> t -> bool

    val add : ?state:State.t -> t -> t -> t

    val sub : ?state:State.t -> t -> t -> t

    val mul : ?state:State.t -> t -> t -> t

    val div : Text.sx -> ?state:State.t -> t -> t -> t

    val logand : ?state:State.t -> t -> t -> t

    val logor : ?state:State.t -> t -> t -> t
  end

  val pp : State.t -> Value0.t Fmt.t
end) =
struct
  module Stack = State.Stack
  module Locals = State.Locals
  module I32 = Value.I32
  module I64 = Value.I64
  (* type state = *)
  (*   { ctx : D.Context.t *)
  (*   ; stack : Stack.t *)
  (*   ; locals : Value.t Locals.t *)
  (*   ; func_rt : val_type list *)
  (*   ; env : Abs_extern_func.extern_func Link_env.t *)
  (*   ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t *)
  (*   } *)

  let ( let* ) = Option.bind

  module Flags = Operator.Flags

  (* let pp_state : Format.formatter -> state -> unit = *)
  (*  fun fmt state -> *)
  (*   Fmt.pf fmt "{@\n@[<hov 2>  ctx : %a,@;stack : %a,@;locals : %a@]@\n}" *)
  (*     D.context_pretty state.ctx *)
  (*     (Stack.pp @@ Value.pp state.ctx) *)
  (*     state.stack *)
  (* (Fmt.list ~sep:Fmt.semi (Value.pp state.ctx)) *)
  (*     (Locals.to_list state.locals |> List.map snd) *)

  module type DATA_STATE = sig
    type t = State.t option * instr option

    val eval_instr : State.t -> instr -> t
  end

  (*===========================================================================*)

  module DenotFixpoint (S : DATA_STATE) = struct
    open Value

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

    (* let serialize ~widens : *)
    (*   State.t -> State.t -> (State.t, 'a) State.Context.result = *)
    (*  fun state_a state_b -> *)
    (*   let gen_new_value ~widens a b state_a state_b *)
    (*     (D.Context.Result (inc, intup, cont)) f = *)
    (*     if Abs_value.equal a b then None *)
    (*     else *)
    (*       let size = Abs_value.size_of a in *)
    (*       (* inc : whether the new value is included in the old one *)
    (*        * intup : symbolic repr of all variabls that will be created simultaneously *)
    (*        * cont : continuation function *)
    (*        *) *)
    (*       let (D.Context.Result (inc, in_tup, local_cont)) = *)
    (*         D.serialize_binary ~size ~widens state_a.ctx (Abs_value.to_binary a) *)
    (*           state_b.ctx (Abs_value.to_binary b) (inc, intup) *)
    (*       in *)
    (*       let cont ctx out_tuple = *)
    (*         let integer, out_tuple = local_cont ctx out_tuple in *)
    (*         let list, out_tuple = cont ctx out_tuple in *)
    (*         let b = Abs_value.of_binary size integer in *)
    (*         (f b list, out_tuple) *)
    (*       in *)
    (*       Some (D.Context.Result (inc, in_tup, cont)) *)
    (*   in *)
    (*   let rec serialize_stack lhs rhs acc_res = *)
    (*     match (lhs, rhs) with *)
    (*     | [], [] -> acc_res *)
    (*     | [], _ :: _ | _ :: _, [] -> *)
    (*       Fmt.failwith "join on stacks of different sizes" *)
    (*     | v1 :: rest_a, v2 :: rest_b -> begin *)
    (*       let r = *)
    (*         gen_new_value ~widens v1 v2 state_a state_b acc_res List.cons *)
    (*       in *)
    (*       serialize_stack rest_a rest_b *)
    (*         (match r with Some res -> res | None -> acc_res) *)
    (*       end *)
    (*   in *)
    (*   let (D.Context.Result (included, in_tuple, locals_continue)) = *)
    (*     Locals.fold_on_nonequal_union *)
    (*       begin fun k v1 v2 res -> *)
    (*         let size = *)
    (*           (* v1 and v2 should have the same size *) *)
    (*           match v1 with *)
    (*           | Some v -> Abs_value.size_of v *)
    (*           | None -> assert false *)
    (*         in *)
    (*         let v1 = *)
    (*           Option.value v1 ~default:(Abs_value.top size state_a.ctx) *)
    (*         in *)
    (*         let v2 = *)
    (*           Option.value v2 ~default:(Abs_value.top size state_b.ctx) *)
    (*         in *)
    (*         let f = Locals.add k in *)
    (*         match gen_new_value ~widens v1 v2 state_a state_b res f with *)
    (*         | Some res -> res *)
    (*         | None -> res *)
    (*       end *)
    (*       state_a.locals state_b.locals *)
    (*       (D.Context.Result *)
    (*          ( true *)
    (*          , D.Context.empty_tuple () *)
    (*          , fun _ctx out -> (state_a.locals, out) ) ) *)
    (*   in *)
    (*   let (D.Context.Result (inc, in_tup, stack_continue)) = *)
    (*     serialize_stack *)
    (*       (Stack.to_list state_a.stack) *)
    (*       (Stack.to_list state_b.stack) *)
    (*       (D.Context.Result (included, in_tuple, fun _ctx out -> ([], out))) *)
    (*   in *)
    (*   let cont ctx out = *)
    (*     let stack, out = stack_continue ctx out in *)
    (*     let locals, out = locals_continue ctx out in *)
    (*     ( { state_a with ctx; stack = Stack.of_list @@ List.rev stack; locals } *)
    (*     , out ) *)
    (*   in *)
    (*   D.Context.Result (inc, in_tup, cont) *)
    (**)
    (* let join state_a state_b = *)
    (*   let (D.Context.Result (_inc, in_tuple, continue)) = *)
    (*     serialize ~widens:false state_a state_b *)
    (*   in *)
    (*   let ctx, out = D.typed_nondet2 state_a.ctx state_b.ctx in_tuple in *)
    (*   fst @@ continue ctx out *)
    (**)
    (* let widen widening_id state_a state_b = *)
    (*   let (D.Context.Result (included, in_tuple, continue)) = *)
    (*     serialize ~widens:true state_a state_b *)
    (*   in *)
    (*   let ctx, included, out = *)
    (*     D.widened_fixpoint_step ~widening_id ~previous:state_a.ctx *)
    (*       ~next:state_b.ctx (included, in_tuple) *)
    (*   in *)
    (*   let state, _ = continue ctx out in *)
    (*   ({ state with ctx }, included) *)

    let rec eval_expr :
         State.t
      -> instr Annotated.t list
      -> State.t option * State.t list JumpTarget.t =
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

    and eval_func (state : State.t) (func : Func.t) =
      Log.info (fun m ->
        m "calling func  : func %s" (Option.value func.id ~default:"anonymous") );
      let (Bt_raw ((None | Some _), (param_type, result_type))) = func.type_f in
      let args, caller_popped_stack =
        Stack.pop_n state.stack (List.length param_type)
      in
      let init_value (vt : val_type) =
        (* let size = *)
        (*   match vt with *)
        (*   | Num_type t -> ( *)
        (*     match t with I32 -> Size.b32 | I64 -> Size.b64 | _ -> assert false ) *)
        (*   | Ref_type _ -> assert false *)
        (* in *)
        (* let zero = D.Binary_Forward.biconst ~size Z.zero state.ctx in *)
        match vt with
        | Num_type I32 -> Value0.I32 (I32.zero ~state)
        | Num_type I64 -> I64 (I64.zero ~state)
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
      Log.info (fun m -> m "Func start state : %a" State.pp fn_state);
      (* TODO: handle mapping *)
      let func_end_state, _ = eval_expr fn_state func.body.raw in
      Log.info (fun m ->
        m "Func end state : %a@."
          (Fmt.option ~none:(Fmt.any "None") State.pp)
          func_end_state );
      (* We should probably copy state and join back the return values in the context here *)
      let* func_end_state in
      let stack =
        Stack.append caller_popped_stack
        @@ Stack.take (List.length result_type) func_end_state.stack
      in
      Some { state with stack }

    and eval_instr :
      State.t -> instr -> State.t option * State.t list JumpTarget.t =
     fun state instr ->
      Log.debug (fun m ->
        m "#%a\t\t%a@." (pp_instr ~short:true) instr State.pp state );
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
          (* | 0 -> *)
          (*   (* assume that it's correctly typed to returning an i32 *) *)
          (*   let v = D.binary_unknown ~size:Size.b32 state.ctx in *)
          (*   let stack = Stack.push state.stack (I32 v) in *)
          (*   (Some { state with stack }, JumpTarget.empty) *)
          (* | 1 -> *)
          (*   (* assume that it's correctly typed to returning an i64 *) *)
          (*   let v = D.binary_unknown ~size:Size.b64 state.ctx in *)
          (*   let stack = Stack.push state.stack (I64 v) in *)
          (*   (Some { state with stack }, JumpTarget.empty) *)
          | _ ->
            Fmt.failwith
              "Some day we will have proper external function support" )
        end
      | Block (_str_opt, _bt, expr) ->
        (* TODO il faut gérer le mapping pour le cas de None *)
        let> res, mapping = eval_expr state expr.raw in
        let state = { state with stack = res.stack; locals = res.locals } in
        let new_state =
          match JumpTarget.find_opt (I 0) mapping with
          | Some br_states -> List.fold_left State.join state br_states
          | None -> state
        in
        let mapping =
          (* TODO on peut avoir une paire de (int * map) pour ne pas avoir à decr la liste immédiatement *)
          JumpTarget.decr mapping
        in
        (Some new_state, mapping)
      | If_else (_, bt, expr_then, expr_else) ->
        let b, stack = Stack.pop_i32 state.stack in
        let cond = Boolean.of_i32 ~state b in
        let state_then, jt_true =
          let> state, _ = (State.assume state cond, JumpTarget.empty) in
          eval_instr { state with stack } (Block (None, bt, expr_then))
        in
        let state_else, jt_false =
          let not_cond = Boolean.not ~state cond in
          let> state, _ = (State.assume state not_cond, JumpTarget.empty) in
          eval_instr { state with stack } (Block (None, bt, expr_else))
        in
        let jt = JumpTarget.append jt_true jt_false in
        begin match (state_then, state_else) with
        | Some state_true, Some state_false ->
          (Some (State.join state_true state_false), jt)
        | Some state, None | None, Some state -> (Some state, jt)
        | None, None ->
          (* TODO should this be assert false ? *)
          (None, jt)
        end
      | Loop (_str_opt, bt, body) ->
        let widening_id = Domains.Sig.Widening_Id.fresh () in
        (* TODO tester si on a besoin de copie *)
        let initial_state = state in
        (* let initial_state = { state with ctx = D.Context.copy state.ctx } in *)
        let rec fixpoint state =
          let next_state, jt = eval_expr state body.raw in
          let to_take =
            match bt with
            | Some (Bt_raw (_i, (params, _res))) -> List.length params
            | None -> 0
          in
          let shorten_stack stack = Stack.take to_take stack in
          let next_head =
            let fp_stack = shorten_stack initial_state.stack in
            match JumpTarget.find_opt (I 0) jt with
            | Some jts ->
              List.fold_left
                (fun acc (state : State.t) ->
                  let stack : State.Stack.t = shorten_stack state.stack in
                  State.join acc { state with stack } )
                { initial_state with stack = fp_stack }
                jts
            | None -> { initial_state with stack = fp_stack }
          in
          let widened, included = State.widen widening_id state next_head in
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
    type t = State.t option * instr option

    (* module Binop = struct *)
    (*   (* TODO vérifier les overflows *) *)
    (*   let binop stack size op = *)
    (*     let e1, e2, stack = Stack.pop_2 stack in *)
    (*     let lhs, rhs = (Abs_value.to_binary e1, Abs_value.to_binary e2) in *)
    (*     let bin_res = op lhs rhs in *)
    (*     let r = Abs_value.of_binary size bin_res in *)
    (*     let stack = Stack.push stack r in *)
    (*     stack *)
    (**)
    (*   let add (state : State.t) size = *)
    (*     let flags = Flags.Biadd.no_overflow in *)
    (*     let op = D.Binary_Forward.biadd ~flags ~size state.ctx in *)
    (*     let stack = binop state.stack size op in *)
    (*     { state with stack } *)
    (**)
    (*   let sub (state : State.t) size = *)
    (*     let flags = Operator.Flags.Bisub.no_overflow in *)
    (*     let op = D.Binary_Forward.bisub ~flags ~size state.ctx in *)
    (*     let stack = binop state.stack size op in *)
    (*     { state with stack } *)
    (**)
    (*   let mul (state : State.t) size = *)
    (*     let flags = Flags.Bimul.pack ~nsw:true ~nuw:true in *)
    (*     let op = D.Binary_Forward.bimul ~flags ~size state.ctx in *)
    (*     let stack = binop state.stack size op in *)
    (*     { state with stack } *)
    (**)
    (*   let div (state : State.t) (sx : Text.sx) size = *)
    (*     let op = *)
    (*       match sx with *)
    (*       | S -> D.Binary_Forward.bisdiv ~size state.ctx *)
    (*       | U -> D.Binary_Forward.biudiv ~size state.ctx *)
    (*     in *)
    (*     let stack = binop state.stack size op in *)
    (*     { state with stack } *)
    (* end *)
    (**)
    (* module Relop = struct *)
    (*   let relop ?(not = None) (state : State.t) size op = *)
    (*     let e1, e2, stack = Stack.pop_2 state.stack in *)
    (*     let lhs, rhs = (Abs_value.to_binary e1, Abs_value.to_binary e2) in *)
    (*     let bool_res = op lhs rhs in *)
    (*     let bool_res = *)
    (*       match not with *)
    (*       | Some v -> *)
    (*         D.Boolean_Forward.( && ) state.ctx *)
    (*           (D.Boolean_Forward.not state.ctx v) *)
    (*           bool_res *)
    (*       | None -> bool_res *)
    (*     in *)
    (*     let r = Abs_value.of_boolean state.ctx size bool_res in *)
    (*     let stack = Stack.push stack r in *)
    (*     stack *)
    (**)
    (*   let _and (state : State.t) size = *)
    (*     let op = D.Binary_Forward.band ~size state.ctx in *)
    (*     let stack = Binop.binop state.stack size op in *)
    (*     { state with stack } *)
    (**)
    (*   let _or (state : State.t) size = *)
    (*     let op = D.Binary_Forward.bor ~size state.ctx in *)
    (*     let stack = Binop.binop state.stack size op in *)
    (*     { state with stack } *)
    (**)
    (*   let le (state : State.t) (sx : Text.sx) size = *)
    (*     let op = *)
    (*       match sx with *)
    (*       | S -> D.Binary_Forward.bisle ~size state.ctx *)
    (*       | U -> D.Binary_Forward.biule ~size state.ctx *)
    (*     in *)
    (*     let stack = relop state size op in *)
    (*     { state with stack } *)
    (**)
    (*   let lt (state : State.t) (sx : Text.sx) size = *)
    (*     let op = *)
    (*       match sx with *)
    (*       | S -> D.Binary_Forward.bisle ~size state.ctx *)
    (*       | U -> D.Binary_Forward.biule ~size state.ctx *)
    (*     in *)
    (*     let stack = relop state size op in *)
    (*     { state with stack } *)
    (* end *)

    let eval_i32 (state : State.t) (instr : Binary.i32_instr) =
      let apply_2 (f : ?state:State.t -> _) =
        let i1, i2, stack = Stack.pop2_i32 state.stack in
        let stack = Value0.I32 (f ~state i1 i2) :: stack in
        { state with stack }
      in
      match instr with
      | Const i ->
        let stack = Stack.push state.stack (I32 (I32.of_concrete i)) in
        { state with stack }
      (* | Const i -> *)
      (*   let abs_i = D.Binary_Forward.biconst ~size (Z.of_int32 i) state.ctx in *)
      (*   let stack = Stack.push state.stack (Abs_value.I32 abs_i) in *)
      (*   { state with stack } *)
      | Add -> apply_2 I32.add
      | Sub -> apply_2 I32.sub
      | Mul -> apply_2 I32.mul
      | Div sx -> apply_2 (I32.div sx)
      | And -> apply_2 I32.logand
      | Or -> apply_2 I32.logor
      (* | Lt sx -> apply_2 (Relop.lt sx) *)
      (* | Le sx -> apply_2 (Relop.le sx) *)
      | _ ->
        Fmt.epr "not implemented yet";
        assert false

    let eval_i64 (state : State.t) (instr : Binary.i64_instr) =
      let apply_2 (f : ?state:State.t -> _) =
        match state.stack with
        | I64 i1 :: I64 i2 :: tl ->
          let stack = Value0.I64 (f ~state i1 i2) :: tl in
          { state with stack }
        | _ -> assert false
      in
      match instr with
      | Const i ->
        let stack = Stack.push state.stack (I64 (I64.of_concrete i)) in
        { state with stack }
      | Add -> apply_2 I64.add
      | Sub -> apply_2 I64.sub
      | Mul -> apply_2 I64.mul
      | Div sx -> apply_2 (I64.div sx)
      | _ ->
        Fmt.epr "not implemented yet";
        assert false

    let eval_local (state : State.t) : Binary.local_instr -> _ = function
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

    let eval_instr : State.t -> instr -> t =
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
end

(* let expr (link_state : Abs_extern_func.extern_func Link.State.t) *)
(*   (m : Abs_extern_func.extern_func Linked.Module.t) = *)
(*   let envs = Link.State.get_envs link_state in *)
(*   let ctx = D.root_context () in *)
(*   let initial_state = *)
(*     { ctx *)
(*     ; stack = Stack.empty *)
(*     ; locals = Locals.empty *)
(*     ; env = m.env *)
(*     ; func_rt = [] *)
(*     ; envs *)
(*     } *)
(*   in *)
(**)
(*   List.iter *)
(*     begin fun (e : expr Annotated.t) -> *)
(*       let end_state, _ = ConcreteFixpoint.eval_expr initial_state e.raw in *)
(*       Fmt.pr "End State : %a@." *)
(*         (Fmt.option ~none:(Fmt.any "none") State.pp) *)
(*         end_state *)
(*     end *)
(*     m.to_run *)

module Dom = Abstract_domain_intv
module Value0 = Abstract_value0
module Value = Abs_value

module State = struct end

include M (Value0) (State) (Value)
