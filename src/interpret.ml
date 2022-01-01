open Types
open Simplify

type env =
  { modules : module_ Array.t
  ; registered_modules : (string, int) Hashtbl.t
  }

let empty_env = { modules = [||]; registered_modules = Hashtbl.create 64 }

exception Return of Stack.t

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
            if n1 = 0x80000000l && n2 = -1l then raise (Trap "integer overflow");
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
      | Shl -> shift_left n1 (to_int n2)
      | Shr s -> begin
        let n2 = to_int n2 in
        match s with S -> shift_right n1 n2 | U -> shift_right_logical n1 n2
      end
      | Rotl -> rotl n1 n2
      | Rotr -> rotr n1 n2)
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
            if n1 = 0x8000000000000000L && n2 = -1L then
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
      | Shl -> shift_left n1 (to_int n2)
      | Shr s -> begin
        let n2 = to_int n2 in
        match s with S -> shift_right n1 n2 | U -> shift_right_logical n1 n2
      end
      | Rotl -> rotl n1 n2
      | Rotr -> rotr n1 n2)

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
      | Copysign -> copy_sign f1 f2)
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
      | Copysign -> copy_sign f1 f2)

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
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt S -> lt_s n1 n2
      | Lt U -> lt_u n1 n2
      | Gt S -> gt_s n1 n2
      | Gt U -> gt_u n1 n2
      | Le S -> le_s n1 n2
      | Le U -> le_u n1 n2
      | Ge S -> ge_s n1 n2
      | Ge U -> ge_u n1 n2
    in
    Stack.push_bool stack res
  | S64 ->
    let (n1, n2), stack = Stack.pop2_i64 stack in
    let res =
      let open Int64 in
      match op with
      | Eq -> eq n1 n2
      | Ne -> ne n1 n2
      | Lt S -> lt_s n1 n2
      | Lt U -> lt_u n1 n2
      | Gt S -> gt_s n1 n2
      | Gt U -> gt_u n1 n2
      | Le S -> le_s n1 n2
      | Le U -> le_u n1 n2
      | Ge S -> ge_s n1 n2
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

exception Branch of Stack.t * int

let fmt = Format.std_formatter

let indice_to_int = function
  | Raw i -> Uint32.to_int i
  | Symbolic id ->
    failwith @@ Format.sprintf "interpreter internal error: unbound id $%s" id

let get_bt = function
  | Bt_ind ind ->
    failwith
    @@ Format.asprintf "internal error: unbound block_type %a" Pp.indice ind
  | Bt_raw (pt, rt) -> (pt, rt)

let init_local (_id, t) =
  match t with
  | Num_type I32 -> Const_I32 Int32.zero
  | Num_type I64 -> Const_I64 Int64.zero
  | Num_type F32 -> Const_F32 Float32.zero
  | Num_type F64 -> Const_F64 Float64.zero
  | Ref_type rt -> Const_null rt

let rec exec_instr env module_indice locals stack instr =
  Debug.debug fmt "stack        : [ %a ]@." Stack.pp stack;
  Debug.debug fmt "running instr: %a@." Pp.instr instr;
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
        (* TODO: is demote correct here ? *)
        let n = Convert.Int32.reinterpret_f32 (Convert.Float32.demote_f64 n) in
        Stack.push_i32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_f32 stack in
        (* TODO: is promote correct here ? *)
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
        (* TODO: is Int64.to_int correct here ? *)
        let n = Convert.Float32.reinterpret_i32 (Int64.to_int32 n) in
        Stack.push_f32 stack n
    end
    | S64 -> begin
      match nn' with
      | S32 ->
        let n, stack = Stack.pop_i32 stack in
        (* TODO: is Int64.of_int32 correct here ? *)
        let n = Convert.Float64.reinterpret_i64 (Int64.of_int32 n) in
        Stack.push_f64 stack n
      | S64 ->
        let n, stack = Stack.pop_i64 stack in
        let n = Convert.Float64.reinterpret_i64 n in
        Stack.push_f64 stack n
    end
  end
  | Ref_null t -> Stack.push stack (Const_null t)
  | Ref_is_null ->
    let b, stack = Stack.pop_is_null stack in
    Stack.push_bool stack b
  | Ref_func i ->
    let i = indice_to_int i in
    Stack.push_host stack i
  | Drop -> Stack.drop stack
  | Local_get i -> Stack.push stack locals.(indice_to_int i)
  | Local_set i ->
    (* TODO: check type ? *)
    let v, stack = Stack.pop stack in
    locals.(indice_to_int i) <- v;
    stack
  | If_else (_id, bt, e1, e2) ->
    let b, stack = Stack.pop_bool stack in
    exec_expr env module_indice locals stack (if b then e1 else e2) false bt
  | Call i ->
    let module_ = env.modules.(module_indice) in
    let func = module_.funcs.(indice_to_int i) in
    let param_type, _result_type = get_bt func.type_f in
    let args, stack = Stack.pop_n stack (List.length param_type) in
    let res = exec_func env module_indice func (List.rev args) in
    res @ stack
  | Br i -> raise (Branch (stack, indice_to_int i))
  | Br_if i ->
    let b, stack = Stack.pop_bool stack in
    if b then raise (Branch (stack, indice_to_int i)) else stack
  | Loop (_id, bt, e) -> exec_expr env module_indice locals stack e true bt
  | Block (_id, bt, e) -> exec_expr env module_indice locals stack e false bt
  | Memory_size ->
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let len = Bytes.length mem / page_size in
    Stack.push_i32_of_int stack len
  | Memory_grow -> (
    let memories = env.modules.(module_indice).memories in
    let mem, max = memories.(0) in
    let delta, stack = Stack.pop_i32_to_int stack in
    let delta = delta * page_size in
    let old_size = Bytes.length mem in
    let new_size = old_size + delta in
    if new_size >= page_size * page_size then Stack.push_i32 stack (-1l)
    else
      match max with
      | None ->
        memories.(0) <- (Bytes.extend mem 0 delta, None);
        Stack.push_i32_of_int stack (old_size / page_size)
      | Some max ->
        if new_size > max * page_size then Stack.push_i32 stack (-1l)
        else begin
          memories.(0) <- (Bytes.extend mem 0 delta, Some max);
          Stack.push_i32_of_int stack (old_size / page_size)
        end )
  | Memory_fill ->
    let start, stack = Stack.pop_i32_to_int stack in
    let byte, stack = Stack.pop_i32_to_int stack in
    let stop, stack = Stack.pop_i32_to_int stack in
    let memories = env.modules.(module_indice).memories in
    let mem, _max = memories.(0) in
    Bytes.fill mem start (stop - start) (Char.chr byte);
    stack
  | Memory_copy -> failwith "TODO Memory_copy"
  | Memory_init _i -> failwith "TODO Memory_init"
  | Select _t ->
    (* TODO: check that o1 and o2 have type t *)
    let b, stack = Stack.pop_bool stack in
    let o2, stack = Stack.pop stack in
    let o1, stack = Stack.pop stack in
    Stack.push stack (if b then o1 else o2)
  | Local_tee i ->
    let v, stack = Stack.pop stack in
    locals.(indice_to_int i) <- v;
    Stack.push stack v
  | Global_get i ->
    let i = indice_to_int i in
    let globals = env.modules.(module_indice).globals in
    let (_mut, _typ), e = globals.(i) in
    Stack.push stack e
  | Global_set i -> (
    let i = indice_to_int i in
    let (mut, typ), _e = env.modules.(module_indice).globals.(i) in
    if mut = Const then failwith "Can't set const global";
    match typ with
    | Ref_type rt ->
      failwith @@ Format.asprintf "TODO global set ref type `%a`" Pp.ref_type rt
    | Num_type I32 ->
      let v, stack = Stack.pop_i32 stack in
      env.modules.(module_indice).globals.(i) <- ((mut, typ), Const_I32 v);
      stack
    | Num_type nt ->
      failwith @@ Format.asprintf "TODO global set num type `%a`" Pp.num_type nt
    )
  | Table_get indice ->
    let indice = indice_to_int indice in
    let t, table, _max = env.modules.(module_indice).tables.(indice) in
    let indice, stack = Stack.pop_i32_to_int stack in
    let v =
      match table.(indice) with
      | exception Invalid_argument _ ->
        raise @@ Trap "out of bounds table access"
      | None -> Const_null t
      | Some v -> v
    in
    Stack.push stack v
  | Table_set indice ->
    let indice = indice_to_int indice in
    let _t, table, _max = env.modules.(module_indice).tables.(indice) in
    let v, stack = Stack.pop stack in
    let indice, stack = Stack.pop_i32_to_int stack in
    begin
      try table.(indice) <- Some v
      with Invalid_argument _ -> raise @@ Trap "out of bounds table access"
    end;
    stack
  | Table_size indice ->
    let indice = indice_to_int indice in
    let _t, table, _max = env.modules.(module_indice).tables.(indice) in
    Stack.push_i32_of_int stack (Array.length table)
  | Table_grow indice ->
    let indice = indice_to_int indice in
    let t, table, max = env.modules.(module_indice).tables.(indice) in
    let size = Array.length table in
    let delta, stack = Stack.pop_i32_to_int stack in
    let new_size = size + delta in
    let allowed =
      Option.value max ~default:Int.max_int >= new_size
      && new_size >= 0 && new_size >= size
    in
    if not allowed then
      let stack = Stack.drop stack in
      Stack.push_i32_of_int stack (-1)
    else
      let new_element, stack = Stack.pop stack in
      let new_table = Array.make new_size (Some new_element) in
      Array.iteri (fun i x -> new_table.(i) <- x) table;
      env.modules.(module_indice).tables.(indice) <- (t, new_table, max);
      Stack.push_i32_of_int stack size
  | Table_fill _ -> failwith "TODO Table_fill"
  | Table_copy _ -> failwith "TODO Table_copy"
  | Table_init (t_indice, e_indice) ->
    let t_indice = indice_to_int t_indice in
    let _t_t, table, _max = env.modules.(module_indice).tables.(t_indice) in
    let e_indice = indice_to_int e_indice in
    let _e_t, e_e = env.modules.(module_indice).elements.(e_indice) in
    let n, stack = Stack.pop_i32_to_int stack in
    let s, stack = Stack.pop_i32_to_int stack in
    let _d, stack = Stack.pop_i32_to_int stack in
    (*if s + n > Array.length e_e || d + n > Array.length table then
        raise @@ Trap "TODO";
      if e_t <> t_t then raise @@ Trap "TODO";*)
    ( if n <> 0 then
      try
        let v = e_e.(s) in
        table.(t_indice) <- Some v
      with Invalid_argument _ -> raise @@ Trap "out of bounds table access" );
    stack
  | Elem_drop _ -> failwith "TODO Elem_drop"
  | I_load16 (nn, sx, { offset; align }) -> (
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    (* TODO: use align *)
    ignore align;
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
  | I_load (nn, { offset; align }) -> (
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    (* TODO: use align *)
    ignore align;
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
  | F_load (nn, { offset; align }) -> (
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    (* TODO: use align *)
    ignore align;
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
  | I_store (nn, { offset; align }) -> (
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    ignore align;
    (* TODO: use align *)
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
  | F_store (nn, { offset; align }) -> (
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    ignore align;
    (* TODO: use align *)
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
  | I_load8 (nn, sx, { offset; align }) -> (
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    (* TODO: use align *)
    ignore align;
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = offset + pos in
    if Bytes.length mem < offset + 1 || pos < 0 then
      raise (Trap "out of bounds memory access");
    let res = (if sx = S then Bytes.get_int8 else Bytes.get_uint8) mem offset in
    match nn with
    | S32 -> Stack.push_i32_of_int stack res
    | S64 -> Stack.push_i64_of_int stack res )
  | I64_load32 (sx, { offset; align }) ->
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    (* TODO: use align *)
    ignore align;
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = pos + offset in
    if Bytes.length mem < offset + 4 || pos < 0 then
      raise (Trap "out of bounds memory access");
    let res =
      ( if sx = S then Bytes.get_int32_le
      else (* TODO: why doesn't get_uint32_le exist ? *)
        Bytes.get_int32_le )
        mem offset
    in
    Stack.push_i64_of_int stack (Int32.to_int res)
  | I_store8 (nn, { offset; align }) ->
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    ignore align;
    (* TODO: use align *)
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
  | I_store16 (nn, { offset; align }) ->
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    ignore align;
    (* TODO: use align *)
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
  | I64_store32 { offset; align } ->
    let offset = Uint32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    ignore align;
    (* TODO: use align *)
    let n, stack = Stack.pop_i64 stack in
    let n = Int64.to_int32 n in
    let pos, stack = Stack.pop_i32_to_int stack in
    let offset = offset + pos in
    if Bytes.length mem < offset + 4 || pos < 0 then
      raise (Trap "out of bounds memory access");
    Bytes.set_int32_le mem offset n;
    stack
  | Data_drop _ -> failwith "TODO Data_drop"
  | Br_table (inds, i) ->
    let target, stack = Stack.pop_i32_to_int stack in
    let target =
      if target < 0 || target >= Array.length inds then i else inds.(target)
    in
    raise (Branch (stack, indice_to_int target))
  | Call_indirect (tbl_i, typ_i) ->
    let fun_i, stack = Stack.pop_i32_to_int stack in
    let module_ = env.modules.(module_indice) in
    let tbl_i = indice_to_int tbl_i in
    let rt, a, _max = module_.tables.(tbl_i) in
    assert (rt = Func_ref);
    if fun_i >= Array.length a then raise (Trap "undefined element");
    let i =
      match a.(fun_i) with
      | None ->
        failwith @@ Format.sprintf "unbound function %d at table %d" fun_i tbl_i
      | Some (Const_host id) -> id
      | Some _r -> failwith "invalid type, expected Const_host"
    in
    let func = module_.funcs.(i) in
    assert (func.type_f = typ_i);
    let param_type, _result_type = get_bt func.type_f in
    let args, stack = Stack.pop_n stack (List.length param_type) in
    let res = exec_func env module_indice func (List.rev args) in
    res @ stack

and exec_expr env module_indice locals stack e is_loop bt =
  let pt, rt =
    match bt with
    | None -> (Int.max_int, Int.max_int)
    | Some bt ->
      let pt, rt = get_bt bt in
      (List.length pt, List.length rt)
  in
  let block_stack, stack = Stack.pop_n stack pt in
  let block_stack =
    List.fold_left
      (fun block_stack instr ->
        try exec_instr env module_indice locals block_stack instr with
        | Branch (block_stack, -1) -> block_stack
        | Branch (block_stack, 0) when is_loop ->
          (* TODO: ?
             exec_expr env module_indice locals block_stack e true bt
          *)
          raise
          @@ Branch
               (exec_expr env module_indice locals block_stack e true bt, -1)
        | Branch (block_stack, 0) ->
          let block_stack = Stack.keep block_stack rt in
          let block_stack = block_stack @ stack in
          raise (Branch (block_stack, -1))
        | Branch (block_stack, n) ->
          let block_stack = block_stack @ stack in
          raise (Branch (block_stack, n - 1))
        | Return block_stack ->
          let block_stack = Stack.keep block_stack rt in
          let block_stack = block_stack @ stack in
          raise (Return block_stack) )
      block_stack e
  in
  let block_stack = Stack.keep block_stack rt in
  let stack = block_stack @ stack in
  Debug.debug fmt "stack        : [ %a ]@." Stack.pp stack;
  stack

and exec_func env module_indice func args =
  Debug.debug fmt "calling func : module %d, func %s@." module_indice
    (Option.value func.id ~default:"anonymous");
  let locals = Array.of_list @@ args @ List.map init_local func.locals in
  try
    exec_expr env module_indice locals [] func.body false (Some func.type_f)
  with
  | Return stack -> stack
  | Branch (stack, -1) -> stack

let invoke env module_indice f args =
  Debug.debug fmt "invoke       : %s@." f;
  let module_ = env.modules.(module_indice) in
  let func_indice =
    match Hashtbl.find_opt module_.exported_funcs f with
    | None -> failwith "undefined export"
    | Some indice -> indice
  in
  let func = module_.funcs.(func_indice) in
  exec_func env module_indice func args

let exec_action env = function
  | Invoke_indice (i, f, args) ->
    let result = invoke env i f args in
    (env, result)
  | Get _ -> failwith "not yet implemented (exec_action)"

let compare_result_const result const =
  match (result, const) with
  | Result_const (Const_I32 n), Const_I32 n' -> n = n'
  | Result_const (Const_I64 n), Const_I64 n' -> n = n'
  | Result_const (Const_F32 n), Const_F32 n' -> n = n'
  | Result_const (Const_F64 n), Const_F64 n' -> n = n'
  | Result_const (Const_null rt), Const_null rt' -> rt = rt'
  | Result_const (Const_host n), Const_host n' -> n = n'
  | Result_const (Const_I32 _), _
  | Result_const (Const_I64 _), _
  | Result_const (Const_F32 _), _
  | Result_const (Const_F64 _), _
  | Result_const (Const_null _), _
  | Result_const (Const_host _), _ ->
    false
  | Result_func_ref, _ -> failwith "TODO (compare_result_const)"
  | Result_extern_ref, _ -> failwith "TODO (compare_result_const)"

let exec_assert env = function
  | SAssert_return (action, results_expected) ->
    Debug.debug fmt "assert return...@.";
    let env, results_got = exec_action env action in
    let results_got = List.rev results_got in
    let eq =
      List.length results_expected = List.length results_got
      && List.for_all2
           (fun result const -> compare_result_const result const)
           results_expected results_got
    in
    if not eq then
      begin
        failwith
        @@ Format.asprintf
             "assert_return failed !@.expected: `%a`@.got     : `%a`@."
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
                Pp.result )
             results_expected Pp.consts results_got
      end;
    env
  | SAssert_trap (action, expected) ->
    Debug.debug fmt "assert trap...@.";
    begin
      try
        let _env, _results = exec_action env action in
        Debug.debug Format.err_formatter "assert_trap failed !@.";
        assert false
      with Trap msg -> assert (msg = expected)
    end;
    env
  | SAssert_trap_module (_module, _expected) -> (* TODO *) env
  | SAssert_exhaustion (_action, _expected) -> (* TODO *) env
  | SAssert_malformed (_mod, _failure) -> (* TODO *) env
  | SAssert_malformed_quote (_mod, _failure) -> (* TODO *) env
  | SAssert_malformed_binary (_mod, _failure) -> (* TODO *) env
  | SAssert_invalid (_mod, _failure) -> (* TODO *) env
  | SAssert_invalid_quote (_mod, _failure) -> (* TODO *) env
  | SAssert_invalid_binary (_mod, _failure) -> (* TODO *) env

let exec_register env name i =
  Hashtbl.replace env.registered_modules name i;
  env

let exec_module env i =
  let module_ = env.modules.(i) in
  Option.iter
    (fun f_id ->
      let _res = exec_func env i module_.funcs.(f_id) [] in
      () )
    module_.start;
  env

let exec_cmd env = function
  | Module_indice i -> exec_module env i
  | Assert a -> exec_assert env a
  | Register_indice (name, i) -> exec_register env name i
  | Action a -> fst (exec_action env a)

let exec script modules =
  let env =
    List.fold_left
      (fun env cmd -> exec_cmd env cmd)
      { empty_env with modules } script
  in
  ignore env
