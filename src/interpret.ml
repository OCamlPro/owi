open Types
open Simplify

type env =
  { modules : module_ Array.t
  ; last_module : int option
  ; seen_modules : (string, int) Hashtbl.t
  ; registered_modules : (string, int) Hashtbl.t
  }

let empty_env =
  { modules = [||]
  ; last_module = None
  ; seen_modules = Hashtbl.create 64
  ; registered_modules = Hashtbl.create 64
  }

exception Return

let exec_iunop stack nn op =
  match nn with
  | S32 ->
    let n = Stack.pop_i32 stack in
    let res =
      match op with
      | Clz -> Op.i32_clz n
      | Ctz -> Op.i32_ctz n
      | Popcnt -> Op.i32_popcnt n
    in
    Stack.push_i32_of_int stack res
  | S64 ->
    let n = Stack.pop_i64 stack in
    let res =
      match op with
      | Clz -> Op.i64_clz n
      | Ctz -> Op.i64_ctz n
      | Popcnt -> Op.i64_popcnt n
    in
    Stack.push_i64_of_int stack res

let exec_funop stack nn op =
  match nn with
  | S32 ->
    let open Float32 in
    let f = Stack.pop_f32 stack in
    let res =
      match op with
      | Abs -> abs f
      | Neg -> neg f
      | Sqrt -> sqrt f
      | Ceil -> ceil f
      | Floor -> floor f
      | Trunc -> trunc f
      | Nearest -> failwith "TODO: exec_funop Nearest"
    in
    Stack.push_f32 stack res
  | S64 ->
    let open Float in
    let f = Stack.pop_f64 stack in
    let res =
      match op with
      | Abs -> abs f
      | Neg -> neg f
      | Sqrt -> sqrt f
      | Ceil -> ceil f
      | Floor -> floor f
      | Trunc -> trunc f
      | Nearest -> failwith "TODO: exec_funop Nearest"
    in
    Stack.push_f64 stack res

exception Trap of string

let exec_ibinop stack nn (op : Types.ibinop) =
  match nn with
  | S32 ->
    let n1, n2 = Stack.pop2_i32 stack in
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
        with
        | Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | Rem s -> begin
        try
          match s with
          | S -> rem n1 n2
          | U -> unsigned_rem n1 n2
        with
        | Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | And -> logand n1 n2
      | Or -> logor n1 n2
      | Xor -> logxor n1 n2
      | Shl -> shift_left n1 (to_int n2)
      | Shr s -> begin
        let n2 = to_int n2 in
        match s with
        | S -> shift_right n1 n2
        | U -> shift_right_logical n1 n2
      end
      | Rotl -> failwith "TODO Rotl"
      | Rotr -> failwith "TODO Rotr")
  | S64 ->
    let n1, n2 = Stack.pop2_i64 stack in
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
        with
        | Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | Rem s -> begin
        try
          match s with
          | S -> rem n1 n2
          | U -> unsigned_rem n1 n2
        with
        | Division_by_zero -> raise (Trap "integer divide by zero")
      end
      | And -> logand n1 n2
      | Or -> logor n1 n2
      | Xor -> logxor n1 n2
      | Shl -> shift_left n1 (to_int n2)
      | Shr s -> begin
        let n2 = to_int n2 in
        match s with
        | S -> shift_right n1 n2
        | U -> shift_right_logical n1 n2
      end
      | Rotl -> failwith "TODO Rotl"
      | Rotr -> failwith "TODO Rotr")

let exec_fbinop stack nn (op : Types.fbinop) =
  match nn with
  | S32 ->
    let f1, f2 = Stack.pop2_f32 stack in
    Stack.push_f32 stack
      (let open Float32 in
      match op with
      | Add -> add f1 f2
      | Sub -> sub f1 f2
      | Mul -> mul f1 f2
      | Div -> div f1 f2
      | Min -> min f1 f2
      | Max -> max f1 f2
      | Copysign -> copysign f1 f2)
  | S64 ->
    let f1, f2 = Stack.pop2_f64 stack in
    Stack.push_f64 stack
      (let open Float in
      match op with
      | Add -> add f1 f2
      | Sub -> sub f1 f2
      | Mul -> mul f1 f2
      | Div -> div f1 f2
      | Min -> min f1 f2
      | Max -> max f1 f2
      | Copysign -> copy_sign f1 f2)

let exec_itestop stack nn op =
  Stack.push_bool stack
    ( match nn with
    | S32 -> (
      let n = Stack.pop_i32 stack in
      match op with
      | Eqz -> n = 0l )
    | S64 -> (
      let n = Stack.pop_i64 stack in
      match op with
      | Eqz -> n = 0L ) )

let exec_irelop stack nn (op : Types.irelop) =
  Stack.push_bool stack
    ( match nn with
    | S32 -> (
      let n1, n2 = Stack.pop2_i32 stack in
      match op with
      | Eq -> n1 = n2
      | Ne -> n1 <> n2
      | Lt _sx -> n1 < n2
      | Gt _sx -> n1 > n2
      | Le _sx -> n1 <= n2
      | Ge _sx -> n1 >= n2 )
    | S64 -> (
      let n1, n2 = Stack.pop2_i64 stack in
      match op with
      | Eq -> n1 = n2
      | Ne -> n1 <> n2
      | Lt _sx -> n1 < n2
      | Gt _sx -> n1 > n2
      | Le _sx -> n1 <= n2
      | Ge _sx -> n1 >= n2 ) )

let exec_frelop stack nn (op : Types.frelop) =
  Stack.push_bool stack
    ( match nn with
    | S32 -> (
      let n1, n2 = Stack.pop2_f32 stack in
      match op with
      | Eq -> n1 = n2
      | Ne -> n1 <> n2
      | Lt -> n1 < n2
      | Gt -> n1 > n2
      | Le -> n1 <= n2
      | Ge -> n1 >= n2 )
    | S64 -> (
      let n1, n2 = Stack.pop2_f64 stack in
      match op with
      | Eq -> n1 = n2
      | Ne -> n1 <> n2
      | Lt -> n1 < n2
      | Gt -> n1 > n2
      | Le -> n1 <= n2
      | Ge -> n1 >= n2 ) )

exception Branch of int

let fmt = Format.std_formatter

let indice_to_int = function
  | Raw i -> Unsigned.UInt32.to_int i
  | Symbolic id ->
    failwith @@ Format.sprintf "interpreter internal error: unbound id %s" id

let init_local (_id, t) =
  match t with
  | Num_type I32 -> Const_I32 0l
  | Num_type I64 -> Const_I64 0L
  | Num_type F32 -> Const_F32 Float32.zero
  | Num_type F64 -> Const_F64 0.
  | Ref_type rt -> Const_null rt

let rec exec_instr env module_indice locals stack instr =
  Debug.debug fmt "stack        : [ %a ]@." Stack.pp stack;
  Debug.debug fmt "running instr: %a@." Pp.instr instr;
  match instr with
  | Return -> raise Return
  | Nop -> ()
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
  | I_extend8_s _n -> failwith "TODO exec_instr"
  | I_extend16_s _n -> failwith "TODO exec_instr"
  | I64_extend32_s -> failwith "TODO exec_instr"
  | I32_wrap_i64 -> failwith "TODO exec_instr"
  | I64_extend_i32 _s -> failwith "TODO exec_instr"
  | I_trunc_f (_n, _n', _s) -> failwith "TODO exec_instr"
  | I_trunc_sat_f (_n, _n', _s) -> failwith "TODO exec_instr"
  | F32_demote_f64 -> failwith "TODO exec_instr"
  | F64_promote_f32 -> failwith "TODO exec_instr"
  | F_convert_i (_n, _n', _s) -> failwith "TODO exec_instr"
  | I_reinterpret_f (_n, _n') -> failwith "TODO exec_instr"
  | F_reinterpret_i (_n, _n') -> failwith "TODO exec_instr"
  | Ref_null t -> Stack.push stack (Const_null t)
  | Ref_is_null ->
    let b = Stack.pop_is_null stack in
    Stack.push_bool stack b
  | Ref_func i ->
    let i = indice_to_int i in
    Stack.push_host stack i
  | Drop -> Stack.drop stack
  | Local_get i -> Stack.push stack locals.(indice_to_int i)
  | Local_set i ->
    (* TODO: check type ? *)
    let v = Stack.pop stack in
    locals.(indice_to_int i) <- v
  | If_else (_bt, e1, e2) ->
    let b = Stack.pop_bool stack in
    exec_expr env module_indice locals stack
      ( if b then
        e1
      else
        e2 )
      false
  | Call i ->
    let module_ = env.modules.(module_indice) in
    let func = module_.funcs.(indice_to_int i) in
    let param_type, _result_type =
      match func.type_f with
      | FTId _i -> failwith "internal error: FTId was not simplified"
      | FTFt (p, r) -> (p, r)
    in
    let args = Stack.pop_n stack (List.length param_type) in
    let res = exec_func env module_indice func args in
    List.iter (Stack.push stack) (List.rev res)
  | Br i -> raise (Branch (indice_to_int i))
  | Br_if i ->
    let b = Stack.pop_bool stack in
    if b then raise (Branch (indice_to_int i))
  | Loop (_bt, e) -> exec_expr env module_indice locals stack e true
  | Block (_bt, e) -> exec_expr env module_indice locals stack e false
  | Memory_size ->
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let len = Bytes.length !mem / page_size in
    Stack.push_i32_of_int stack len
  | Memory_grow -> (
    let mem, max = env.modules.(module_indice).memories.(0) in
    let delta = Stack.pop_i32_to_int stack * page_size in
    let old_size = Bytes.length !mem in
    match max with
    | None ->
      mem := Bytes.extend !mem 0 delta;
      Stack.push_i32_of_int stack (old_size / page_size)
    | Some max ->
      if old_size + delta > max * page_size then
        Stack.push_i32 stack (-1l)
      else begin
        mem := Bytes.extend !mem 0 delta;
        Stack.push_i32_of_int stack (old_size / page_size)
      end )
  | Memory_fill ->
    let start = Stack.pop_i32_to_int stack in
    let byte = Stack.pop_i32_to_int stack in
    let stop = Stack.pop_i32_to_int stack in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    Bytes.fill !mem start (stop - start) (Char.chr byte)
  | Memory_copy -> failwith "TODO Memory_copy"
  | Memory_init _i -> failwith "TODO Memory_init"
  | Select _t ->
    let b = Stack.pop_bool stack in
    let o2 = Stack.pop stack in
    let o1 = Stack.pop stack in
    Stack.push stack
      ( if b then
        o1
      else
        o2 )
  | Local_tee i ->
    let v = Stack.pop stack in
    locals.(indice_to_int i) <- v;
    Stack.push stack v
  | Global_get i -> (
    let i = indice_to_int i in
    let (_mut, _typ), e = env.modules.(module_indice).globals.(i) in
    match e with
    | [] -> failwith "empty global expr"
    | [ I32_const n ] -> Stack.push_i32 stack n
    | _ -> failwith @@ Format.asprintf "TODO global_get expr: `%a`" Pp.expr e )
  | Global_set i -> (
    let i = indice_to_int i in
    let (mut, typ), _e = env.modules.(module_indice).globals.(i) in
    if mut = Const then failwith "Can't set const global";
    match typ with
    | Ref_type rt ->
      failwith @@ Format.asprintf "TODO global set ref type `%a`" Pp.ref_type rt
    | Num_type I32 ->
      let v = Stack.pop_i32 stack in
      env.modules.(module_indice).globals.(i) <- ((mut, typ), [ I32_const v ])
    | Num_type nt ->
      failwith @@ Format.asprintf "TODO global set num type `%a`" Pp.num_type nt
    )
  | Table_get indice ->
    let indice = indice_to_int indice in
    let t, table, _max = env.modules.(module_indice).tables.(indice) in
    let indice = Stack.pop_i32_to_int stack in
    let v =
      match table.(indice) with
      | exception Invalid_argument _ ->
        raise @@ Trap "out of bounds table access"
      | None -> Const_null t
      | Some v -> v
    in
    Stack.push stack v
  | Table_set indice -> (
    let indice = indice_to_int indice in
    let _t, table, _max = env.modules.(module_indice).tables.(indice) in
    let v = Stack.pop stack in
    let indice = Stack.pop_i32_to_int stack in
    try table.(indice) <- Some v with
    | Invalid_argument _ -> raise @@ Trap "out of bounds table access" )
  | Table_size indice ->
    let indice = indice_to_int indice in
    let _t, table, _max = env.modules.(module_indice).tables.(indice) in
    Stack.push_i32_of_int stack (Array.length table)
  | Table_grow indice ->
    let indice = indice_to_int indice in
    let t, table, max = env.modules.(module_indice).tables.(indice) in
    let size = Array.length table in
    let new_size = size + Stack.pop_i32_to_int stack in
    let allowed =
      Option.value max ~default:Int.max_int >= new_size
      && new_size >= 0 && new_size >= size
    in
    if not allowed then begin
      Stack.drop stack;
      Stack.push_i32_of_int stack (-1)
    end else
      let new_element = Stack.pop stack in
      let new_table = Array.make new_size (Some new_element) in
      Array.iteri (fun i x -> new_table.(i) <- x) table;
      env.modules.(module_indice).tables.(indice) <- (t, new_table, max);
      Stack.push_i32_of_int stack size
  | Table_fill _ -> failwith "TODO Table_fill"
  | Table_copy _ -> failwith "TODO Table_copy"
  | Table_init _ -> failwith "TODO Table_init"
  | Elem_drop _ -> failwith "TODO Elem_drop"
  | I_load16 (nn, sx, { offset; align }) -> (
    let offset = Unsigned.UInt32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let mem = !mem in
    (* TODO: use align *)
    ignore align;
    let pos = Stack.pop_i32_to_int stack in
    let offset = pos + offset in
    if Bytes.length mem < offset + 2 || pos < 0 then
      raise (Trap "out of bounds memory access");
    let res =
      ( if sx = S then
        Bytes.get_int16_le
      else
        Bytes.get_uint16_le )
        mem offset
    in
    match nn with
    | S32 -> Stack.push_i32_of_int stack res
    | S64 -> Stack.push_i64_of_int stack res )
  | I_load (nn, { offset; align }) -> (
    let offset = Unsigned.UInt32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let mem = !mem in
    (* TODO: use align *)
    ignore align;
    let pos = Stack.pop_i32_to_int stack in
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
    let offset = Unsigned.UInt32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let mem = !mem in
    (* TODO: use align *)
    ignore align;
    let pos = Stack.pop_i32_to_int stack in
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
      let res = Int64.float_of_bits res in
      Stack.push_f64 stack res )
  | I_store (nn, { offset; align }) -> (
    let offset = Unsigned.UInt32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let mem = !mem in
    ignore align;
    (* TODO: use align *)
    match nn with
    | S32 ->
      let n = Stack.pop_i32 stack in
      let pos = Stack.pop_i32_to_int stack in
      let offset = offset + pos in
      if Bytes.length mem < offset + 4 || pos < 0 then
        raise (Trap "out of bounds memory access");
      Bytes.set_int32_le mem offset n
    | S64 ->
      let n = Stack.pop_i64 stack in
      let pos = Stack.pop_i32_to_int stack in
      let offset = offset + pos in
      if Bytes.length mem < offset + 8 || pos < 0 then
        raise (Trap "out of bounds memory access");
      Bytes.set_int64_le mem offset n )
  | F_store (_, _) -> failwith "TODO F_store"
  | I_load8 (nn, sx, { offset; align }) -> (
    let offset = Unsigned.UInt32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let mem = !mem in
    (* TODO: use align *)
    ignore align;
    let pos = Stack.pop_i32_to_int stack in
    let offset = offset + pos in
    if Bytes.length mem < offset + 1 || pos < 0 then
      raise (Trap "out of bounds memory access");
    match nn with
    | S32 ->
      let res =
        ( if sx = S then
          Bytes.get_int8
        else
          Bytes.get_uint8 )
          mem offset
      in
      Stack.push_i32_of_int stack res
    | S64 ->
      let res =
        ( if sx = S then
          Bytes.get_int8
        else
          Bytes.get_uint8 )
          mem offset
      in
      Stack.push_i64_of_int stack res )
  | I64_load32 (sx, { offset; align }) ->
    let offset = Unsigned.UInt32.to_int offset in
    let mem, _max = env.modules.(module_indice).memories.(0) in
    let mem = !mem in
    (* TODO: use align *)
    ignore align;
    let pos = Stack.pop_i32_to_int stack in
    let offset = pos + offset in
    if Bytes.length mem < offset + 4 || pos < 0 then
      raise (Trap "out of bounds memory access");
    let res =
      ( if sx = S then
        Bytes.get_int32_le
      else
        (* TODO: why doesn't get_uint32_le exist ? *)
        Bytes.get_int32_le )
        mem offset
    in
    Stack.push_i64_of_int stack (Int32.to_int res)
  | I_store8 (_, _) -> failwith "TODO I_store8"
  | I_store16 (_, _) -> failwith "TODO I_store16"
  | I64_store32 _ -> failwith "TODO I64"
  | Data_drop _ -> failwith "TODO Data_drop"
  | Br_table (inds, i) ->
    let target = Stack.pop_i32_to_int stack in
    let target =
      if target < 0 || target >= Array.length inds then
        i
      else
        inds.(target)
    in
    raise (Branch (indice_to_int target))
  | Call_indirect (tbl_i, typ_i) ->
    let fun_i = Stack.pop_i32_to_int stack in
    let module_ = env.modules.(module_indice) in
    let tbl_i = indice_to_int tbl_i in
    let rt, a, _max = module_.tables.(tbl_i) in
    if rt <> Func_ref then failwith "wrong table type (expected Func_ref)";
    if fun_i >= Array.length a then raise (Trap "undefined element");
    let i =
      match a.(fun_i) with
      | None ->
        failwith @@ Format.sprintf "unbound function %d at table %d" fun_i tbl_i
      | Some (Const_host id) -> id
      | Some _r -> failwith "invalid type, expected Const_host"
    in
    let func = module_.funcs.(i) in
    if func.type_f <> typ_i then
      failwith
      @@ Format.asprintf "Invalid Call_indirect type: `%a` <> `%a`"
           Pp.func_type_bis func.type_f Pp.func_type_bis typ_i;
    let param_type =
      match func.type_f with
      | FTId i ->
        failwith
        @@ Format.asprintf "internal error: FTId `%a` was not simplified"
             Pp.indice i
      | FTFt (p, _r) -> p
    in
    let args = Stack.pop_n stack (List.length param_type) in
    let res = exec_func env module_indice func args in
    List.iter (Stack.push stack) (List.rev res)

and exec_expr env module_indice locals stack e is_loop =
  List.iter
    (fun instr ->
      try exec_instr env module_indice locals stack instr with
      | Branch -1 -> ()
      | Branch 0 when is_loop ->
        exec_expr env module_indice locals stack e true (* TODO: -1 ? *)
      | Branch n -> raise (Branch (n - 1)) )
    e

and exec_func env module_indice func args =
  Debug.debug fmt "calling func : module %d, func %s@." module_indice
    (Option.value func.id ~default:"anonymous");
  let locals = Array.of_list @@ args @ List.map init_local func.locals in
  let stack = Stack.create () in
  let result =
    try
      exec_expr env module_indice locals stack func.body false;
      stack
    with
    | Return
    | Branch -1 ->
      stack
  in
  let return_type =
    match func.type_f with
    | FTId _id -> failwith "internal error"
    | FTFt (_pt, rt) -> rt
  in
  Stack.keep stack (List.length return_type);
  Debug.debug fmt "stack        : [ %a ]@." Stack.pp result;
  Stack.to_list result

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
  match result with
  | Result_const (Const_F64 f) -> begin
    match const with
    | Const_F64 f2 -> Float.equal f f2
    | _ -> false
  end
  | Result_const c -> const = c
  | Result_func_ref -> failwith "TODO (compare_result_const)"
  | Result_extern_ref -> failwith "TODO (compare_result_const)"

let exec_assert env = function
  | SAssert_return (action, results_expected) ->
    Debug.debug fmt "assert return...@.";
    (* TODO: why do we have to rev here, is it correct ? *)
    let results_expected = List.rev results_expected in
    let env, results_got = exec_action env action in
    let eq =
      List.length results_expected = List.length results_got
      && List.for_all2
           (fun result const -> compare_result_const result const)
           results_expected results_got
    in
    if not eq then
      Debug.debug Format.err_formatter
        "assert_return failed !@.expected: `%a`@.got     : `%a`@."
        (Format.pp_print_list Pp.result)
        results_expected Pp.consts results_got;
    env
  | SAssert_trap (action, expected) ->
    Debug.debug fmt "assert trap...@.";
    begin
      try
        let _env, _results = exec_action env action in
        Debug.debug Format.err_formatter "assert_trap failed !@.";
        assert false
      with
      | Trap msg -> assert (msg = expected)
    end;
    env
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
  let curr_func = ref (-1) in
  let seen_funcs = env.modules.(i).seen_funcs in
  List.iter
    (function
      | MFunc f ->
        incr curr_func;
        Option.iter (fun id -> Hashtbl.replace seen_funcs id !curr_func) f.id
      | _ -> () )
    env.modules.(i).fields;
  { env with last_module = Some i }

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
