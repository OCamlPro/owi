open Types
open Simplified

let dummy_pos =
  let dummy_pos = { Wasm.Source.file = ""; line = 0; column = 0 } in
  { Wasm.Source.left = dummy_pos; right = dummy_pos }

let at region x = { Wasm.Source.at = region; it = x }

let indice i = at dummy_pos @@ Int32.of_int i

let mut = function Const -> Wasm.Types.Cons | Var -> Wasm.Types.Var

let num_i64 (x : Int64.t) : Wasm.Value.num = Wasm.Value.I64 x

let num_i32 (x : Int32.t) : Wasm.Value.num = Wasm.Value.I32 x

let num_f32 (x : Float32.t) : Wasm.Value.num =
  Wasm.Value.F32 (Wasm.F32.of_bits @@ Float32.to_bits x)

let num_f64 (x : Float64.t) : Wasm.Value.num =
  Wasm.Value.F64 (Wasm.F64.of_bits @@ Float64.to_bits x)

let num_type = function
  | I32 -> Wasm.Types.I32T
  | I64 -> Wasm.Types.I64T
  | F32 -> Wasm.Types.F32T
  | F64 -> Wasm.Types.F64T

let heap_type = function
  | Func_ht -> Wasm.Types.FuncHT
  | Extern_ht -> Wasm.Types.ExternHT
  | _ -> assert false

let nullable = function Null -> Wasm.Types.Null | No_null -> Wasm.Types.NoNull

let value_type = function
  | Num_type nt -> Wasm.Types.NumT (num_type nt)
  | Ref_type (null, ht) -> Wasm.Types.RefT (nullable null, heap_type ht)

let param (_id, vt) = at dummy_pos @@ { Wasm.Ast.ltype = value_type vt }

let const_instr = function
  | Const.I32_const n -> Wasm.Ast.Const (at dummy_pos @@ num_i32 n)
  | Const.I64_const n -> Wasm.Ast.Const (at dummy_pos @@ num_i64 n)
  | Const.F32_const n -> Wasm.Ast.Const (at dummy_pos @@ num_f32 n)
  | Const.F64_const n -> Wasm.Ast.Const (at dummy_pos @@ num_f64 n)
  | i ->
    Format.eprintf "TODO: To_reference.const_instr for %a@\n"
      Simplified.Pp.Pp_const.instr i;
    assert false

let const_expr = List.map (fun i -> at dummy_pos @@ const_instr i)

let iunop = function
  | Clz -> Wasm.Ast.IntOp.Clz
  | Ctz -> Wasm.Ast.IntOp.Ctz
  | Popcnt -> Wasm.Ast.IntOp.Popcnt

let ibinop = function
  | (Add : ibinop) -> Wasm.Ast.IntOp.Add
  | Sub -> Wasm.Ast.IntOp.Sub
  | Mul -> Wasm.Ast.IntOp.Mul
  | And -> Wasm.Ast.IntOp.And
  | Or -> Wasm.Ast.IntOp.Or
  | Xor -> Wasm.Ast.IntOp.Xor
  | Shl -> Wasm.Ast.IntOp.Shl
  | Rotl -> Wasm.Ast.IntOp.Rotr
  | Rotr -> Wasm.Ast.IntOp.Rotr
  | Div S -> Wasm.Ast.IntOp.DivS
  | Div U -> Wasm.Ast.IntOp.DivU
  | Rem S -> Wasm.Ast.IntOp.RemS
  | Rem U -> Wasm.Ast.IntOp.RemU
  | Shr S -> Wasm.Ast.IntOp.ShrS
  | Shr U -> Wasm.Ast.IntOp.ShrU

let irelop = function
  | (Eq : irelop) -> Wasm.Ast.IntOp.Eq
  | Ne -> Wasm.Ast.IntOp.Ne
  | Lt S -> Wasm.Ast.IntOp.LtS
  | Lt U -> Wasm.Ast.IntOp.LtU
  | Gt S -> Wasm.Ast.IntOp.GtS
  | Gt U -> Wasm.Ast.IntOp.GtU
  | Le S -> Wasm.Ast.IntOp.LeS
  | Le U -> Wasm.Ast.IntOp.LeU
  | Ge S -> Wasm.Ast.IntOp.GeS
  | Ge U -> Wasm.Ast.IntOp.GeU

let itestop = function Eqz -> Wasm.Ast.IntOp.Eqz

let funop = function
  | Abs -> Wasm.Ast.FloatOp.Abs
  | Neg -> Wasm.Ast.FloatOp.Neg
  | Sqrt -> Wasm.Ast.FloatOp.Sqrt
  | Ceil -> Wasm.Ast.FloatOp.Ceil
  | Floor -> Wasm.Ast.FloatOp.Floor
  | Trunc -> Wasm.Ast.FloatOp.Trunc
  | Nearest -> Wasm.Ast.FloatOp.Nearest

let fbinop = function
  | Add -> Wasm.Ast.FloatOp.Add
  | Sub -> Wasm.Ast.FloatOp.Sub
  | Mul -> Wasm.Ast.FloatOp.Mul
  | Div -> Wasm.Ast.FloatOp.Div
  | Min -> Wasm.Ast.FloatOp.Min
  | Max -> Wasm.Ast.FloatOp.Max
  | Copysign -> Wasm.Ast.FloatOp.CopySign

let frelop = function
  | (Eq : frelop) -> Wasm.Ast.FloatOp.Eq
  | Ne -> Wasm.Ast.FloatOp.Ne
  | Lt -> Wasm.Ast.FloatOp.Lt
  | Gt -> Wasm.Ast.FloatOp.Gt
  | Le -> Wasm.Ast.FloatOp.Le
  | Ge -> Wasm.Ast.FloatOp.Ge

let block_type = function
  | None -> Wasm.Ast.ValBlockType None
  | Some (_pt, _rt) ->
    let vt = (* TODO: replace by pt or rt *) value_type (Num_type I32) in
    Wasm.Ast.ValBlockType (Some vt)

let iload nn memarg =
  let ty = if nn = S32 then Wasm.Types.I32T else I64T in
  let align = memarg.align in
  let offset = Int32.of_int memarg.offset in
  let pack = None in
  { Wasm.Ast.ty; align; offset; pack }

let istore nn memarg =
  let ty = if nn = S32 then Wasm.Types.I32T else I64T in
  let align = memarg.align in
  let offset = Int32.of_int memarg.offset in
  let pack = None in
  { Wasm.Ast.ty; align; offset; pack }

let fload nn memarg =
  let ty = if nn = S32 then Wasm.Types.F32T else F64T in
  let align = memarg.align in
  let offset = Int32.of_int memarg.offset in
  let pack = None in
  { Wasm.Ast.ty; align; offset; pack }

let fstore nn memarg =
  let ty = if nn = S32 then Wasm.Types.F32T else F64T in
  let align = memarg.align in
  let offset = Int32.of_int memarg.offset in
  let pack = None in
  { Wasm.Ast.ty; align; offset; pack }

let rec instr = function
  | I32_const n -> Wasm.Ast.Const (at dummy_pos @@ num_i32 n)
  | I64_const n -> Wasm.Ast.Const (at dummy_pos @@ num_i64 n)
  | F32_const n -> Wasm.Ast.Const (at dummy_pos @@ num_f32 n)
  | F64_const n -> Wasm.Ast.Const (at dummy_pos @@ num_f64 n)
  | Nop -> Wasm.Ast.Nop
  | I_unop (S32, op) -> Wasm.Ast.(Unary (I32 (iunop op)))
  | I_binop (S32, op) -> Wasm.Ast.(Binary (I32 (ibinop op)))
  | F_unop (S32, op) -> Wasm.Ast.(Unary (F32 (funop op)))
  | F_binop (S32, op) -> Wasm.Ast.(Binary (F32 (fbinop op)))
  | I_unop (S64, op) -> Wasm.Ast.(Unary (I64 (iunop op)))
  | I_binop (S64, op) -> Wasm.Ast.(Binary (I64 (ibinop op)))
  | F_unop (S64, op) -> Wasm.Ast.(Unary (F64 (funop op)))
  | F_binop (S64, op) -> Wasm.Ast.(Binary (F64 (fbinop op)))
  | I_extend16_s nn ->
    if nn = S32 then Wasm.Ast.(Unary (I32 (ExtendS Wasm.Pack.Pack16)))
    else Wasm.Ast.(Unary (I64 (ExtendS Wasm.Pack.Pack16)))
  | Block (_id, bt, e) ->
    let bt = block_type bt in
    let e = expr e in
    Wasm.Ast.Block (bt, e)
  | Loop (_id, bt, e) ->
    let bt = block_type bt in
    let e = expr e in
    Wasm.Ast.Loop (bt, e)
  | I_load (nn, memarg) -> Wasm.Ast.(Load (iload nn memarg))
  | I_store (nn, memarg) -> Wasm.Ast.(Store (istore nn memarg))
  | F_load (nn, memarg) -> Wasm.Ast.(Load (fload nn memarg))
  | F_store (nn, memarg) -> Wasm.Ast.(Store (fstore nn memarg))
  | I_store8 (nn, memarg) ->
    let ty = if nn = S32 then Wasm.Types.I32T else I64T in
    let align = memarg.align in
    let offset = Int32.of_int memarg.offset in
    let pack = Some Wasm.Pack.Pack8 in
    Wasm.Ast.Store { Wasm.Ast.ty; align; offset; pack }
  | I_store16 (nn, memarg) ->
    let ty = if nn = S32 then Wasm.Types.I32T else I64T in
    let align = memarg.align in
    let offset = Int32.of_int memarg.offset in
    let pack = Some Wasm.Pack.Pack16 in
    Wasm.Ast.Store { Wasm.Ast.ty; align; offset; pack }
  | I64_store32 memarg ->
    let ty = Wasm.Types.I64T in
    let align = memarg.align in
    let offset = Int32.of_int memarg.offset in
    let pack = Some Wasm.Pack.Pack32 in
    Wasm.Ast.Store { Wasm.Ast.ty; align; offset; pack }
  | I_load8 (nn, sx, memarg) ->
    let ty = if nn = S32 then Wasm.Types.I32T else I64T in
    let align = memarg.align in
    let offset = Int32.of_int memarg.offset in
    let pack =
      Some (Wasm.Pack.Pack8, if sx = S then Wasm.Pack.SX else Wasm.Pack.ZX)
    in
    Wasm.Ast.Load { Wasm.Ast.ty; align; offset; pack }
  | I_load16 (nn, sx, memarg) ->
    let ty = if nn = S32 then Wasm.Types.I32T else I64T in
    let align = memarg.align in
    let offset = Int32.of_int memarg.offset in
    let pack =
      Some (Wasm.Pack.Pack16, if sx = S then Wasm.Pack.SX else Wasm.Pack.ZX)
    in
    Wasm.Ast.Load { Wasm.Ast.ty; align; offset; pack }
  | I64_load32 (sx, memarg) ->
    let ty = Wasm.Types.I64T in
    let align = memarg.align in
    let offset = Int32.of_int memarg.offset in
    let pack =
      Some (Wasm.Pack.Pack32, if sx = S then Wasm.Pack.SX else Wasm.Pack.ZX)
    in
    Wasm.Ast.Load { Wasm.Ast.ty; align; offset; pack }
  (* TODO: src float not used ?! *)
  (* TODO: src float not used ?! *)
  | I_reinterpret_f (S32, S32) -> Wasm.Ast.(Convert (I32 ReinterpretFloat))
  | I_reinterpret_f (S32, S64) -> Wasm.Ast.(Convert (I64 ReinterpretFloat))
  | I_reinterpret_f (S64, S32) -> Wasm.Ast.(Convert (I32 ReinterpretFloat))
  | I_reinterpret_f (S64, S64) -> Wasm.Ast.(Convert (I64 ReinterpretFloat))
  | I_trunc_f (S32, S32, S) -> Wasm.Ast.(Convert (I32 TruncSF32))
  | I_trunc_f (S32, S32, U) -> Wasm.Ast.(Convert (I32 TruncUF32))
  | I_trunc_f (S32, S64, S) -> Wasm.Ast.(Convert (I32 TruncSF64))
  | I_trunc_f (S32, S64, U) -> Wasm.Ast.(Convert (I32 TruncUF64))
  | I_trunc_f (S64, S32, S) -> Wasm.Ast.(Convert (I64 TruncSF32))
  | I_trunc_f (S64, S32, U) -> Wasm.Ast.(Convert (I64 TruncUF32))
  | I_trunc_f (S64, S64, S) -> Wasm.Ast.(Convert (I64 TruncSF64))
  | I_trunc_f (S64, S64, U) -> Wasm.Ast.(Convert (I64 TruncUF64))
  | I_trunc_sat_f (S32, S32, S) -> Wasm.Ast.(Convert (I32 TruncSatSF32))
  | I_trunc_sat_f (S32, S32, U) -> Wasm.Ast.(Convert (I32 TruncSatUF32))
  | I_trunc_sat_f (S32, S64, S) -> Wasm.Ast.(Convert (I32 TruncSatSF64))
  | I_trunc_sat_f (S32, S64, U) -> Wasm.Ast.(Convert (I32 TruncSatUF64))
  | I_trunc_sat_f (S64, S32, S) -> Wasm.Ast.(Convert (I64 TruncSatSF32))
  | I_trunc_sat_f (S64, S32, U) -> Wasm.Ast.(Convert (I64 TruncSatUF32))
  | I_trunc_sat_f (S64, S64, S) -> Wasm.Ast.(Convert (I64 TruncSatSF64))
  | I_trunc_sat_f (S64, S64, U) -> Wasm.Ast.(Convert (I64 TruncSatUF64))
  | I_extend8_s S32 -> Wasm.Ast.(Unary (I32 (ExtendS Wasm.Pack.Pack8)))
  | I_extend8_s S64 -> Wasm.Ast.(Unary (I64 (ExtendS Wasm.Pack.Pack8)))
  | I32_wrap_i64 -> Wasm.Ast.(Convert (I32 WrapI64))
  | F32_demote_f64 -> Wasm.Ast.(Convert (F32 DemoteF64))
  | F64_promote_f32 -> Wasm.Ast.(Convert (F64 PromoteF32))
  (* TODO: missing src size in reference interpret *)
  | F_reinterpret_i (S32, S32) -> Wasm.Ast.(Convert (F32 ReinterpretInt))
  | F_reinterpret_i (S32, S64) -> Wasm.Ast.(Convert (F32 ReinterpretInt))
  | F_reinterpret_i (S64, S32) -> Wasm.Ast.(Convert (F64 ReinterpretInt))
  | F_reinterpret_i (S64, S64) -> Wasm.Ast.(Convert (F64 ReinterpretInt))
  | F_convert_i (S32, S32, S) -> Wasm.Ast.(Convert (F32 ConvertSI32))
  | F_convert_i (S32, S32, U) -> Wasm.Ast.(Convert (F32 ConvertUI32))
  | F_convert_i (S32, S64, S) -> Wasm.Ast.(Convert (F32 ConvertSI64))
  | F_convert_i (S32, S64, U) -> Wasm.Ast.(Convert (F32 ConvertUI64))
  | F_convert_i (S64, S32, S) -> Wasm.Ast.(Convert (F64 ConvertSI32))
  | F_convert_i (S64, S32, U) -> Wasm.Ast.(Convert (F64 ConvertUI32))
  | F_convert_i (S64, S64, S) -> Wasm.Ast.(Convert (F64 ConvertSI64))
  | F_convert_i (S64, S64, U) -> Wasm.Ast.(Convert (F64 ConvertUI64))
  | I64_extend_i32 S -> Wasm.Ast.(Convert (I64 ExtendSI32))
  | I64_extend_i32 U -> Wasm.Ast.(Convert (I64 ExtendUI32))
  | I64_extend32_s -> Wasm.Ast.(Unary (I64 (ExtendS Wasm.Pack.Pack32)))
  | I_relop (S32, op) -> Wasm.Ast.(Compare (I32 (irelop op)))
  | I_relop (S64, op) -> Wasm.Ast.(Compare (I64 (irelop op)))
  | F_relop (S32, op) -> Wasm.Ast.(Compare (F32 (frelop op)))
  | F_relop (S64, op) -> Wasm.Ast.(Compare (F64 (frelop op)))
  | I_testop (S32, op) -> Wasm.Ast.(Test (I32 (itestop op)))
  | I_testop (S64, op) -> Wasm.Ast.(Test (I64 (itestop op)))
  | Br ind -> Wasm.Ast.Br (indice ind)
  | Br_if ind -> Wasm.Ast.BrIf (indice ind)
  | Local_get ind -> Wasm.Ast.LocalGet (indice ind)
  | Local_set ind -> Wasm.Ast.LocalSet (indice ind)
  | Local_tee ind -> Wasm.Ast.LocalTee (indice ind)
  | Global_get ind -> Wasm.Ast.LocalGet (indice ind)
  | Global_set ind -> Wasm.Ast.LocalSet (indice ind)
  | Call ind -> Wasm.Ast.Call (indice ind)
  | If_else (_id, bt, e1, e2) ->
    let e1 = expr e1 in
    let e2 = expr e2 in
    Wasm.Ast.If (block_type bt, e1, e2)
  | Unreachable -> Wasm.Ast.Unreachable
  | Drop -> Wasm.Ast.Drop
  | Memory_size -> Wasm.Ast.MemorySize
  | Data_drop ind -> Wasm.Ast.DataDrop (indice ind)
  | Memory_grow -> Wasm.Ast.MemoryGrow
  | i ->
    Format.eprintf "TODO: To_reference.instr for %a@\n" Simplified.Pp.instr i;
    assert false

and expr e = List.map (fun i -> at dummy_pos @@ instr i) e

let limits l =
  { Wasm.Types.min = Int32.of_int l.min; max = Option.map Int32.of_int l.max }

let of_global ({ typ; init; id = _ } : global) =
  let m, t = typ in
  let m = mut m in
  let t = value_type t in
  let gtype = Wasm.Types.GlobalT (m, t) in
  let ginit = at dummy_pos @@ const_expr init in
  at dummy_pos @@ { Wasm.Ast.gtype; ginit }

let of_table _ = assert false

let of_memory (_id, l) =
  let mtype = Wasm.Types.MemoryT (limits l) in
  at dummy_pos @@ { Wasm.Ast.mtype }

let of_func ({ type_f = _; locals; body; id = _ } : func) : Wasm.Ast.func =
  let ftype = at dummy_pos 0l (* TODO: use type_f *) in
  let locals = List.map param locals in
  let body = expr body in
  let f = { Wasm.Ast.ftype; locals; body } in
  at dummy_pos @@ f

let of_elem _ = assert false

let of_data _ = assert false

let runtime ~local = function
  | Runtime.Local a -> local a
  | Imported _b -> assert false

let of_runtime_named ~local n =
  Named.fold
    (fun _i x acc ->
      let x = runtime ~local x in
      x :: acc )
    n []

let of_named f n =
  Named.fold
    (fun _i x acc ->
      let x = f x in
      x :: acc )
    n []

let modul { id = _; global; table; mem; func; elem; data; exports = _; start } =
  let types = [] in
  let globals = of_runtime_named ~local:of_global global in
  let tables = of_runtime_named ~local:of_table table in
  let memories = of_runtime_named ~local:of_memory mem in
  let funcs = of_runtime_named ~local:of_func func in
  let elems = of_named of_elem elem in
  let datas = of_named of_data data in
  let imports = [] in
  let exports = [] in
  let start =
    Option.map
      (fun id ->
        at dummy_pos { Wasm.Ast.sfunc = at dummy_pos @@ Int32.of_int id } )
      start
  in
  at dummy_pos
  @@ Wasm.Script.Module
       ( None
       , at dummy_pos
         @@ Wasm.Script.Textual
              (at dummy_pos
                 { Wasm.Ast.types
                 ; globals
                 ; tables
                 ; memories
                 ; funcs
                 ; start
                 ; elems
                 ; datas
                 ; imports
                 ; exports
                 } ) )
