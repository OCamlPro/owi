open Types
module Env = Link.Env

exception Return of Env.t' Stack.t

let page_size = 65_536

let p_type_eq (_id1, t1) (_id2, t2) = t1 = t2

let exec_iunop stack nn op =
  match nn with
  | S32 ->
    let n, stack = Stack.pop_i32 stack in
    let res =
      let open Int32 in
      match op with Clz -> clz n | Ctz -> ctz n | Popcnt -> popcnt n
    in
    Stack.push_i32_of_int stack res
  | S64 ->
    let n, stack = Stack.pop_i64 stack in
    let res =
      let open Int64 in
      match op with Clz -> clz n | Ctz -> ctz n | Popcnt -> popcnt n
    in
    Stack.push_i64_of_int stack res

let exec_funop stack nn op =
  match nn with
  | S32 ->
    let open Float32 in
    let f, stack = Stack.pop_f32 stack in
    let res =
      match op with
      | Abs -> abs f
      | Neg -> neg f
      | Sqrt -> sqrt f
      | Ceil -> ceil f
      | Floor -> floor f
      | Trunc -> trunc f
      | Nearest -> nearest f
    in
    Stack.push_f32 stack res
  | S64 ->
    let open Float64 in
    let f, stack = Stack.pop_f64 stack in
    let res =
      match op with
      | Abs -> abs f
      | Neg -> neg f
      | Sqrt -> sqrt f
      | Ceil -> ceil f
      | Floor -> floor f
      | Trunc -> trunc f
      | Nearest -> nearest f
    in
    Stack.push_f64 stack res

let exec_ibinop stack nn (op : Types.ibinop) =
  match nn with
  | S32 ->
    let (n1, n2), stack = Stack.pop2_i32 stack in
    Stack.push_i32 stack
      (let open Int32 in
      match op with
      | Add -> add n1 n2
      | Sub -> sub n1 n2
      | Mul -> mul n1 n2
      | Div s -> begin
        try
          match s with
          | S ->
            if n1 = Int32.min_int && n2 = -1l then
              raise (Trap "integer overflow");
            div n1 n2
          | U -> unsigned_div n1 n2
        with Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | Rem s -> begin
        try match s with S -> rem n1 n2 | U -> unsigned_rem n1 n2
        with Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | And -> logand n1 n2
      | Or -> logor n1 n2
      | Xor -> logxor n1 n2
      | Shl -> shl n1 n2
      | Shr S -> shr_s n1 n2
      | Shr U -> shr_u n1 n2
      | Rotl -> rotl n1 n2
      | Rotr -> rotr n1 n2 )
  | S64 ->
    let (n1, n2), stack = Stack.pop2_i64 stack in
    Stack.push_i64 stack
      (let open Int64 in
      match op with
      | Add -> add n1 n2
      | Sub -> sub n1 n2
      | Mul -> mul n1 n2
      | Div s -> begin
        try
          match s with
          | S ->
            if n1 = Int64.min_int && n2 = -1L then
              raise (Trap "integer overflow");
            div n1 n2
          | U -> unsigned_div n1 n2
        with Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | Rem s -> begin
        try match s with S -> rem n1 n2 | U -> unsigned_rem n1 n2
        with Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | And -> logand n1 n2
      | Or -> logor n1 n2
      | Xor -> logxor n1 n2
      | Shl -> shl n1 n2
      | Shr S -> shr_s n1 n2
      | Shr U -> shr_u n1 n2
      | Rotl -> rotl n1 n2
      | Rotr -> rotr n1 n2 )

let exec_fbinop stack nn (op : Types.fbinop) =
  match nn with
  | S32 ->
    let (f1, f2), stack = Stack.pop2_f32 stack in
    Stack.push_f32 stack
      (let open Float32 in
      match op with
      | Add -> add f1 f2
      | Sub -> sub f1 f2
      | Mul -> mul f1 f2
      | Div -> div f1 f2
      | Min -> min f1 f2
      | Max -> max f1 f2
      | Copysign -> copy_sign f1 f2 )
  | S64 ->
    let (f1, f2), stack = Stack.pop2_f64 stack in
    Stack.push_f64 stack
      (let open Float64 in
      match op with
      | Add -> add f1 f2
      | Sub -> sub f1 f2
      | Mul -> mul f1 f2
      | Div -> div f1 f2
      | Min -> min f1 f2
      | Max -> max f1 f2
      | Copysign -> copy_sign f1 f2 )

let exec_itestop stack nn op =
  match nn with
  | S32 ->
    let n, stack = Stack.pop_i32 stack in
    let res = match op with Eqz -> n = 0l in
    Stack.push_bool stack res
  | S64 ->
    let n, stack = Stack.pop_i64 stack in
    let res = match op with Eqz -> n = 0L in
    Stack.push_bool stack res

let exec_irelop stack nn (op : Types.irelop) =
  match nn with
  | S32 ->
    let (n1, n2), stack = Stack.pop2_i32 stack in
    let res =
      let open Int32 in
      match op with
      | Eq -> n1 = n2
      | Ne -> n1 <> n2
      | Lt S -> n1 < n2
      | Lt U -> lt_u n1 n2
      | Gt S -> n1 > n2
      | Gt U -> gt_u n1 n2
      | Le S -> n1 <= n2
      | Le U -> le_u n1 n2
      | Ge S -> n1 >= n2
      | Ge U -> ge_u n1 n2
    in
    Stack.push_bool stack res
  | S64 ->
    let (n1, n2), stack = Stack.pop2_i64 stack in
    let res =
      let open Int64 in
      match op with
      | Eq -> n1 = n2
      | Ne -> n1 <> n2
      | Lt S -> n1 < n2
      | Lt U -> lt_u n1 n2
      | Gt S -> n1 > n2
      | Gt U -> gt_u n1 n2
      | Le S -> n1 <= n2
      | Le U -> le_u n1 n2
      | Ge S -> n1 >= n2
      | Ge U -> ge_u n1 n2
    in
    Stack.push_bool stack res

let exec_frelop stack nn (op : Types.frelop) =
  match nn with
  | S32 ->
    let (n1, n2), stack = Stack.pop2_f32 stack in
    let res =
      let open Float32 in
      match op with
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt -> lt n1 n2
      | Gt -> gt n1 n2
      | Le -> le n1 n2
      | Ge -> ge n1 n2
    in
    Stack.push_bool stack res
  | S64 ->
    let (n1, n2), stack = Stack.pop2_f64 stack in
    let res =
      let open Float64 in
      match op with
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt -> lt n1 n2
      | Gt -> gt n1 n2
      | Le -> le n1 n2
      | Ge -> ge n1 n2
    in
    Stack.push_bool stack res

exception Branch of Env.t' Stack.t * int

let init_local (_id, t) : Env.t' Value.t =
  match t with
  | Num_type I32 -> I32 Int32.zero
  | Num_type I64 -> I64 Int64.zero
  | Num_type F32 -> F32 Float32.zero
  | Num_type F64 -> F64 Float64.zero
  | Ref_type rt -> Value.ref_null rt

(* TODO move to module Env *)
let mem_0 = 0

let get_memory (env : Env.t) idx =
  let mem = Env.get_memory env idx in
  Link.Memory.(get_data mem, get_limit_max mem)

let get_memory_raw env idx = Env.get_memory env idx

let exec_extern_func stack (f : Value.Func.extern_func) =
  let pop_arg (type ty) stack (arg : ty Value.Func.telt) : ty * Env.t' Stack.t =
    match arg with
    | I32 -> Stack.pop_i32 stack
    | I64 -> Stack.pop_i64 stack
    | F32 -> Stack.pop_f32 stack
    | F64 -> Stack.pop_f64 stack
    | Externref ety -> Stack.pop_as_externref ety stack
  in
  let rec apply :
      type f r.
      Env.t' Stack.t -> (f, r) Value.Func.atype -> f -> r * Env.t' Stack.t =
   fun stack ty f ->
    match ty with
    | Value.Func.Arg (arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | NArg (_, arg, args) ->
      let v, stack = pop_arg stack arg in
      apply stack args (f v)
    | Res -> (f, stack)
  in
  let (Extern_func (Func (atype, rtype), func)) = f in
  let r, stack = apply stack atype func in
  let push_val (type ty) (arg : ty Value.Func.telt) (v : ty) stack =
    match arg with
    | I32 -> Stack.push_i32 stack v
    | I64 -> Stack.push_i64 stack v
    | F32 -> Stack.push_f32 stack v
    | F64 -> Stack.push_f64 stack v
    | Externref ty -> Stack.push_as_externref stack ty v
  in
  match (rtype, r) with
  | R0, () -> stack
  | R1 t1, v1 -> push_val t1 v1 stack
  | R2 (t1, t2), (v1, v2) -> push_val t1 v1 stack |> push_val t2 v2
  | R3 (t1, t2, t3), (v1, v2, v3) ->
    push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3
  | R4 (t1, t2, t3, t4), (v1, v2, v3, v4) ->
    push_val t1 v1 stack |> push_val t2 v2 |> push_val t3 v3 |> push_val t4 v4

let rec exec_instr env locals stack instr =
  Log.debug "stack        : [ %a ]@." Stack.pp stack;
  Log.debug "running instr: %a@." Pp.Simplified.instr instr;
  match instr with
  | Return -> raise @@ Return stack
  | Nop -> stack
  | Unreachable -> raise @@ Trap "unreachable"
  | I32_const n -> Stack.push_i32 stack n
  | I64_const n -> Stack.push_i64 stack n
  | F32_const f -> Stack.push_f32 stack f
  | F64_const f -> Stack.push_f64 stack f
  | I_unop (nn, op) -> exec_iunop stack nn op
  | F_unop (nn, op) -> exec_funop stack nn op
  | I_binop (nn, op) -> exec_ibinop stack nn op
  | F_binop (nn, op) -> exec_fbinop stack nn op
  | I_testop (nn, op) -> exec_itestop stack nn op
  | I_relop (nn, op) -> exec_irelop stack nn op
  | F_relop (nn, op) -> exec_frelop stack nn op
  | I_extend8_s nn -> begin
    match nn with
    | S32 ->
      let n, stack = Stack.pop_i32 stack in
      let n = Int32.extend_s 8 n in
      Stack.push_i32 stack n
    | S64 ->
      let n, stack = Stack.pop_i64 stack in
      let n = Int64.extend_s 8 n in
      Stack.push_i64 stack n
  end
  | I_extend16_s nn -> begin
    match nn with
    | S32 ->
      let n, stack = Stack.pop_i32 stack in
      let n = Int32.extend_s 16 n in
      Stack.push_i32 stack n
    | S64 ->
      let n, stack = Stack.pop_i64 stack in
      let n = Int64.extend_s 16 n in
      Stack.push_i64 stack n
  end
  | I64_extend32_s ->
    let n, stack = Stack.pop_i64 stack in
    let n = Int64.extend_s 32 n in
    Stack.push_i64 stack n
  | I32_wrap_i64 ->
    let n, stack = Stack.pop_i64 stack in
    let n = Convert.Int32.wrap_i64 n in
    Stack.push_i32 stack n
  | I64_extend_i32 s ->
    let n, stack = Stack.pop_i32 stack in
    let n =
      match s with
      | S -> Convert.Int64.extend_i32_s n
      | U -> Convert.Int64.extend_i32_u n
    in
    Stack.push_i64 stack n
  | I_trunc_f (n, n', s) -> (
    match (n, n') with
    | S32, S32 -> (
      let f, stack = Stack.pop_f32 stack in
      Stack.push_i32 stack
      @@
      match s with
      | S -> Convert.Int32.trunc_f32_s f
      | U -> Convert.Int32.trunc_f32_u f )
    | S32, S64 -> (
      let f, stack = Stack.pop_f64 stack in
      Stack.push_i32 stack
      @@
      match s with
      | S -> Convert.Int32.trunc_f64_s f
      | U -> Convert.Int32.trunc_f64_u f )
    | S64, S32 -> (
      let f, stack = Stack.pop_f32 stack in
      Stack.push_i64 stack
      @@
      match s with
      | S -> Convert.Int64.trunc_f32_s f
      | U -> Convert.Int64.trunc_f32_u f )
    | S64, S64 -> (
      let f, stack = Stack.pop_f64 stack in
      Stack.push_i64 stack
      @@
      match s with
      | S -> Convert.Int64.trunc_f64_s f
      | U -> Convert.Int64.trunc_f64_u f ) )
  | I_trunc_sat_f (nn, nn', s) -> begin
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n =
          match s with
          | S -> Convert.Int32.trunc_sat_f32_s n
          | U -> Convert.Int32.trunc_sat_f32_u n
        in
        Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n =
          match s with
          | S -> Convert.Int32.trunc_sat_f64_s n
          | U -> Convert.Int32.trunc_sat_f64_u n
        in
        Stack.push_i32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n =
          match s with
          | S -> Convert.Int64.trunc_sat_f32_s n
          | U -> Convert.Int64.trunc_sat_f32_u n
        in
        Stack.push_i64 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n =
          match s with
          | S -> Convert.Int64.trunc_sat_f64_s n
          | U -> Convert.Int64.trunc_sat_f64_u n
        in
        Stack.push_i64 stack n
    end
  end
  | F32_demote_f64 ->
    let n, stack = Stack.pop_f64 stack in
    let n = Convert.Float32.demote_f64 n in
    Stack.push_f32 stack n
  | F64_promote_f32 ->
    let n, stack = Stack.pop_f32 stack in
    let n = Convert.Float64.promote_f32 n in
    Stack.push_f64 stack n
  | F_convert_i (n, n', s) -> (
    match n with
    | S32 -> (
      let open Convert.Float32 in
      match n' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = if s = S then convert_i32_s n else convert_i32_u n in
        Stack.push_f32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = if s = S then convert_i64_s n else convert_i64_u n in
        Stack.push_f32 stack n )
    | S64 -> (
      let open Convert.Float64 in
      match n' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = if s = S then convert_i32_s n else convert_i32_u n in
        Stack.push_f64 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = if s = S then convert_i64_s n else convert_i64_u n in
        Stack.push_f64 stack n ) )
  | I_reinterpret_f (nn, nn') -> begin
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n = Convert.Int32.reinterpret_f32 n in
        Stack.push_i32 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n = Convert.Int32.reinterpret_f32 (Convert.Float32.demote_f64 n) in
        Stack.push_i32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        let n = Convert.Int64.reinterpret_f64 (Convert.Float64.promote_f32 n) in
        Stack.push_i64 stack n
      | S64 ->
        let n, stack = Stack.pop_f64 stack in
        let n = Convert.Int64.reinterpret_f64 n in
        Stack.push_i64 stack n
    end
  end
  | F_reinterpret_i (nn, nn') -> begin
    match nn with
    | S32 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = Convert.Float32.reinterpret_i32 n in
        Stack.push_f32 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = Convert.Float32.reinterpret_i32 (Int64.to_int32 n) in
        Stack.push_f32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        let n = Convert.Float64.reinterpret_i64 (Int64.of_int32 n) in
        Stack.push_f64 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = Convert.Float64.reinterpret_i64 n in
        Stack.push_f64 stack n
    end
  end
  | Ref_null t -> Stack.push stack (Value.ref_null t)
  | Ref_is_null ->
    let b, stack = Stack.pop_is_null stack in
    Stack.push_bool stack b
  | Ref_func i -> Stack.push stack (Value.ref_func (Env.get_func env i))
  | Drop -> Stack.drop stack
  | Local_get i -> Stack.push stack locals.(i)
  | Local_set i ->
    let v, stack = Stack.pop stack in
    locals.(i) <- v;
    stack
  | If_else (_id, bt, e1, e2) ->
    let b, stack = Stack.pop_bool stack in
    exec_expr env locals stack (if b then e1 else e2) false bt
  | Call i -> begin
    let func = Env.get_func env i in
    exec_vfunc stack func
  end
  | Return_call i -> begin
    let func = get_func env i in
    exec_vfunc ~return:true stack func
  end
  | Br i -> raise (Branch (stack, i))
  | Br_if i ->
    let b, stack = Stack.pop_bool stack in
    if b then raise (Branch (stack, i)) else stack
  | Loop (_id, bt, e) -> exec_expr env locals stack e true bt
  | Block (_id, bt, e) -> exec_expr env locals stack e false bt
  | Memory_size ->
    let mem, _max = get_memory env mem_0 in
    let len = Bytes.length mem / page_size in
    Stack.push_i32_of_int stack len
  | Memory_grow -> begin
    let mem = get_memory_raw env mem_0 in
    let data = Link.Memory.get_data mem in
    let max_size = Link.Memory.get_limit_max mem in
    let delta, stack = Stack.pop_i32_to_int stack in
    let delta = delta * page_size in
    let old_size = Bytes.length data in
    let new_size = old_size + delta in
    if new_size >= page_size * page_size then Stack.push_i32 stack (-1l)
    else
      match max_size with
      | Some max when new_size > max * page_size -> Stack.push_i32 stack (-1l)
      | None | Some _ ->
        let new_mem = Bytes.extend data 0 delta in
        Bytes.fill new_mem old_size delta (Char.chr 0);
        Link.Memory.update_memory mem new_mem;
        Stack.push_i32_of_int stack (old_size / page_size)
  end
  | Memory_fill ->
    let len, stack = Stack.pop_i32_to_int stack in
    let c, stack = Stack.pop_i32_to_char stack in
    let pos, stack = Stack.pop_i32_to_int stack in
    let mem, _max = get_memory env mem_0 in
    begin
      try Bytes.fill mem pos len c
      with Invalid_argument _ -> raise @@ Trap "out of bounds memory access"
    end;
    stack
  | Memory_copy ->
    let mem, _max = get_memory env mem_0 in
    let len, stack = Stack.pop_i32_to_int stack in
    let src_pos, stack = Stack.pop_i32_to_int stack in
    let dst_pos, stack = Stack.pop_i32_to_int stack in
    begin
      try Bytes.blit mem src_pos mem dst_pos len
      with Invalid_argument _ -> raise (Trap "out of bounds memory access")
    end;
    stack
  | Memory_init i ->
    let mem, _max = get_memory env mem_0 in
    let len, stack = Stack.pop_i32_to_int stack in
    let src_pos, stack = Stack.pop_i32_to_int stack in
    let dst_pos, stack = Stack.pop_i32_to_int stack in
    let data = Env.get_data env i in
    ( try Bytes.blit_string data.value src_pos mem dst_pos len
      with Invalid_argument _ -> raise (Trap "out of bounds memory access") );
    stack
  | Select _t ->
    let b, stack = Stack.pop_bool stack in
    let o2, stack = Stack.pop stack in
    let o1, stack = Stack.pop stack in
    Stack.push stack (if b then o1 else o2)
  | Local_tee i ->
    let v, stack = Stack.pop stack in
    locals.(i) <- v;
    Stack.push stack v
  | Global_get i -> Stack.push stack (Env.get_global env i).value
  | Global_set i ->
    let global = Env.get_global env i in
    if global.mut = Const then Log.err "Can't set const global";
    let v, stack =
      match global.typ with
      | Ref_type rt -> begin
        match rt with
        | Extern_ref | Func_ref ->
          let v, stack = Stack.pop_ref stack in
          (v, stack)
      end
      | Num_type nt -> (
        match nt with
        | I32 ->
          let v, stack = Stack.pop_i32 stack in
          (I32 v, stack)
        | I64 ->
          let v, stack = Stack.pop_i64 stack in
          (I64 v, stack)
        | F32 ->
          let v, stack = Stack.pop_f32 stack in
          (F32 v, stack)
        | F64 ->
          let v, stack = Stack.pop_f64 stack in
          (F64 v, stack) )
    in
    global.value <- v;
    stack
  | Table_get indice ->
    let t = Env.get_table env indice in
    let indice, stack = Stack.pop_i32_to_int stack in
    let v =
      match t.data.(indice) with
      | exception Invalid_argument _ ->
        raise @@ Trap "out of bounds table access"
      | v -> v
    in
    Stack.push stack (Ref v)
  | Table_set indice ->
    let t = Env.get_table env indice in
    let v, stack = Stack.pop_as_ref stack in
    let indice, stack = Stack.pop_i32_to_int stack in
    begin
      try t.data.(indice) <- v
      with Invalid_argument _ -> raise @@ Trap "out of bounds table access"
    end;
    stack
  | Table_size indice ->
    let t = Env.get_table env indice in
    Stack.push_i32_of_int stack (Array.length t.data)
  | Table_grow indice ->
    let t = Env.get_table env indice in
    let size = Array.length t.data in
    let delta, stack = Stack.pop_i32_to_int stack in
    let new_size = size + delta in
    let allowed =
      Option.value t.limits.max ~default:Int.max_int >= new_size
      && new_size >= 0 && new_size >= size
    in
    if not allowed then
      let stack = Stack.drop stack in
      Stack.push_i32_of_int stack (-1)
    else
      let new_element, stack = Stack.pop_as_ref stack in
      let new_table = Array.make new_size new_element in
      Array.blit t.data 0 new_table 0 (Array.length t.data);
      Link.Table.update t new_table;
      Stack.push_i32_of_int stack size
  | Table_fill indice ->
    let t = Env.get_table env indice in
    let len, stack = Stack.pop_i32_to_int stack in
    let x, stack = Stack.pop_as_ref stack in
    let pos, stack = Stack.pop_i32_to_int stack in
    begin
      try Array.fill t.data pos len x
      with Invalid_argument _ -> raise @@ Trap "out of bounds table access"
    end;
    stack
  | Table_copy (ti_dst, ti_src) -> begin
    let t_src = Env.get_table env ti_src in
    let t_dst = Env.get_table env ti_dst in
    let len, stack = Stack.pop_i32_to_int stack in
    let src, stack = Stack.pop_i32_to_int stack in
    let dst, stack = Stack.pop_i32_to_int stack in
    if
      src + len > Array.length t_src.data || dst + len > Array.length t_dst.data
    then raise @@ Trap "out of bounds table access";
    if len = 0 then stack
    else
      try
        Array.blit t_src.data src t_dst.data dst len;
        stack
      with Invalid_argument _ -> raise @@ Trap "out of bounds table access"
  end
  | Table_init (t_i, e_i) ->
    let t = Env.get_table env t_i in
    let elem = Env.get_elem env e_i in
    let len, stack = Stack.pop_i32_to_int stack in
    let pos_x, stack = Stack.pop_i32_to_int stack in
    let pos, stack = Stack.pop_i32_to_int stack in
    (* TODO: this is dumb, why do we have to fail even when len = 0 ?
     * I don't remember where exactly but somewhere else it's the opposite:
     * if len is 0 then we do not fail...
     * if it wasn't needed, the following check would be useless
     * as the next one would take care of it
     * (or maybe not because we don't want to fail
     * in the middle of the loop but still...)*)
    if
      pos_x + len > Array.length elem.value
      || pos + len > Array.length t.data
      || 0 > len
    then raise @@ Trap "out of bounds table access";
    begin
      try
        for i = 0 to len - 1 do
          let idx = pos_x + i in
          if idx < 0 || idx >= Array.length elem.value then
            raise @@ Trap "out of bounds table access";
          let x = elem.value.(idx) in
          let idx = pos + i in
          if idx < 0 || idx >= Array.length t.data then
            raise @@ Trap "out of bounds table access";
          Array.set t.data idx x
        done
      with Invalid_argument _ -> raise @@ Trap "out of bounds table access"
    end;
    stack
  | Elem_drop i ->
    let elem = Env.get_elem env i in
    Env.drop_elem elem;
    stack
  | I_load16 (nn, sx, { offset; _ }) -> (
    let mem, _max = get_memory env mem_0 in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = pos + offset in
    if Bytes.length mem < offset + 2 || pos < 0 then
      raise (Trap "out of bounds memory access");
    let res =
      (if sx = S then Bytes.get_int16_le else Bytes.get_uint16_le) mem offset
    in
    match nn with
    | S32 -> Stack.push_i32_of_int stack res
    | S64 -> Stack.push_i64_of_int stack res )
  | I_load (nn, { offset; _ }) -> (
    let mem, _max = get_memory env mem_0 in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = pos + offset in
    match nn with
    | S32 ->
      if Bytes.length mem < offset + 4 || pos < 0 then
        raise (Trap "out of bounds memory access");
      let res = Bytes.get_int32_le mem offset in
      Stack.push_i32 stack res
    | S64 ->
      if Bytes.length mem < offset + 8 || pos < 0 then
        raise (Trap "out of bounds memory access");
      let res = Bytes.get_int64_le mem offset in
      Stack.push_i64 stack res )
  | F_load (nn, { offset; _ }) -> (
    let mem, _max = get_memory env mem_0 in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = pos + offset in
    match nn with
    | S32 ->
      if Bytes.length mem < offset + 4 || pos < 0 then
        raise (Trap "out of bounds memory access");
      let res = Bytes.get_int32_le mem offset in
      let res = Float32.of_bits res in
      Stack.push_f32 stack res
    | S64 ->
      if Bytes.length mem < offset + 8 || pos < 0 then
        raise (Trap "out of bounds memory access");
      let res = Bytes.get_int64_le mem offset in
      let res = Float64.of_bits res in
      Stack.push_f64 stack res )
  | I_store (nn, { offset; _ }) -> (
    let mem, _max = get_memory env mem_0 in
    match nn with
    | S32 ->
      let n, stack = Stack.pop_i32 stack in
      let pos, stack = Stack.pop_i32_to_int stack in
      let offset = offset + pos in
      if Bytes.length mem < offset + 4 || pos < 0 then
        raise (Trap "out of bounds memory access");
      Bytes.set_int32_le mem offset n;
      stack
    | S64 ->
      let n, stack = Stack.pop_i64 stack in
      let pos, stack = Stack.pop_i32_to_int stack in
      let offset = offset + pos in
      if Bytes.length mem < offset + 8 || pos < 0 then
        raise (Trap "out of bounds memory access");
      Bytes.set_int64_le mem offset n;
      stack )
  | F_store (nn, { offset; _ }) -> (
    let mem, _max = get_memory env mem_0 in
    match nn with
    | S32 ->
      let n, stack = Stack.pop_f32 stack in
      let pos, stack = Stack.pop_i32_to_int stack in
      let offset = offset + pos in
      if Bytes.length mem < offset + 4 || pos < 0 then
        raise (Trap "out of bounds memory access");
      Bytes.set_int32_le mem offset (Float32.to_bits n);
      stack
    | S64 ->
      let n, stack = Stack.pop_f64 stack in
      let pos, stack = Stack.pop_i32_to_int stack in
      let offset = offset + pos in
      if Bytes.length mem < offset + 8 || pos < 0 then
        raise (Trap "out of bounds memory access");
      Bytes.set_int64_le mem offset (Float64.to_bits n);
      stack )
  | I_load8 (nn, sx, { offset; _ }) -> (
    let mem, _max = get_memory env mem_0 in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = offset + pos in
    if Bytes.length mem < offset + 1 || pos < 0 then
      raise (Trap "out of bounds memory access");
    let res = (if sx = S then Bytes.get_int8 else Bytes.get_uint8) mem offset in
    match nn with
    | S32 -> Stack.push_i32_of_int stack res
    | S64 -> Stack.push_i64_of_int stack res )
  | I64_load32 (sx, { offset; _ }) ->
    let mem, _max = get_memory env mem_0 in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = pos + offset in
    if Bytes.length mem < offset + 4 || pos < 0 then
      raise (Trap "out of bounds memory access");
    let res = Int32.to_int @@ Bytes.get_int32_le mem offset in
    let res =
      if sx = S || Sys.word_size = 32 then res
      else if Sys.word_size = 64 then Int.(logand res (sub (shift_left 1 32) 1))
      else Log.err "unsupported word size"
    in
    Stack.push_i64_of_int stack res
  | I_store8 (nn, { offset; _ }) ->
    let mem, _max = get_memory env mem_0 in
    let n, stack =
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        (Int32.to_int n, stack)
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        (Int64.to_int n, stack)
    in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = offset + pos in
    if Bytes.length mem < offset + 1 || pos < 0 then
      raise (Trap "out of bounds memory access");
    Bytes.set_int8 mem offset n;
    stack
  | I_store16 (nn, { offset; _ }) ->
    let mem, _max = get_memory env mem_0 in
    let n, stack =
      match nn with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        (Int32.to_int n, stack)
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        (Int64.to_int n, stack)
    in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = offset + pos in
    if Bytes.length mem < offset + 2 || pos < 0 then
      raise (Trap "out of bounds memory access");
    Bytes.set_int16_le mem offset n;
    stack
  | I64_store32 { offset; _ } ->
    let mem, _max = get_memory env mem_0 in
    let n, stack = Stack.pop_i64 stack in
    let n = Int64.to_int32 n in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = offset + pos in
    if Bytes.length mem < offset + 4 || pos < 0 then
      raise (Trap "out of bounds memory access");
    Bytes.set_int32_le mem offset n;
    stack
  | Data_drop i ->
    let data = Env.get_data env i in
    Env.drop_data data;
    stack
  | Br_table (inds, i) ->
    let target, stack = Stack.pop_i32_to_int stack in
    let target =
      if target < 0 || target >= Array.length inds then i else inds.(target)
    in
    raise (Branch (stack, target))
  | Call_indirect (tbl_i, typ_i) ->
    let fun_i, stack = Stack.pop_i32_to_int stack in
    let t = Env.get_table env tbl_i in
    if t.type_ <> Func_ref then raise @@ Trap "indirect call type mismatch";
    let func =
      match t.data.(fun_i) with
      | exception Invalid_argument _ ->
        raise @@ Trap "undefined element" (* fails here *)
      | Funcref (Some f) -> f
      | Funcref None ->
        raise @@ Trap (Printf.sprintf "uninitialized element %i" fun_i)
      | _ -> raise @@ Trap "element type error"
    in
    let pt, rt = Value.Func.typ func in
    let pt', rt' = typ_i in
    if not (rt = rt' && List.equal p_type_eq pt pt') then
      raise @@ Trap "indirect call type mismatch";
    exec_vfunc stack func

and exec_expr env locals stack e is_loop bt =
  let rt =
    Option.fold ~none:Int.max_int ~some:(fun bt -> List.length (snd bt)) bt
  in
  let block_stack =
    try List.fold_left (exec_instr env locals) stack e with
    | Branch (block_stack, 0) ->
      if is_loop then exec_expr env locals block_stack e true bt
      else block_stack
    | Branch (block_stack, n) -> raise (Branch (block_stack, n - 1))
  in
  let stack = Stack.keep block_stack rt @ stack in
  Log.debug "stack        : [ %a ]@." Stack.pp stack;
  stack

and exec_vfunc ?(return=false) stack (func : Env.t' Value.Func.t) =
  match func with
  | WASM (_, func, env) ->
    let param_type, _result_type = func.type_f in
    let args, stack = Stack.pop_n stack (List.length param_type) in
    let env = Lazy.force env in
    let rev_args = List.rev args in
    if return then
      exec_func env func rev_args
    else
      let res = exec_func env func rev_args in
      res @ stack
  | Extern f ->
    let stack = List.rev stack in
    exec_extern_func stack f

and exec_func env func args =
  Log.debug "calling func : func %s@."
    (Option.value func.id ~default:"anonymous");
  let locals = Array.of_list @@ args @ List.map init_local func.locals in
  try exec_expr env locals [] func.body false (Some func.type_f)
  with Return stack -> Stack.keep stack (List.length (snd func.type_f))

let exec_vfunc stack func =
  try Ok (exec_vfunc stack func) with
  | Failure msg | Trap msg -> Error msg
  | Stack_overflow -> Error "call stack exhausted"

let module_ (module_ : Link.module_to_run) =
  Log.debug "interpreting ...@\n";
  try
    List.iter
      (fun to_run ->
        let end_stack =
          exec_expr module_.env [||] Stack.empty to_run false None
        in
        match end_stack with
        | [] -> ()
        | _ :: _ -> Format.eprintf "non empty stack@\n%a@." Stack.pp end_stack
        )
      module_.to_run;
    Ok ()
  with
  | Failure msg | Trap msg -> Error msg
  | Stack_overflow -> Error "call stack exhausted"
