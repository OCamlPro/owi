exception Trap of string

exception Parse_fail of string

type nonrec num_type =
  | I32
  | I64
  | F32
  | F64

type nullable =
  | No_null
  | Null

type nonrec packed_type =
  | I8
  | I16

type nonrec mut =
  | Const
  | Var

type nonrec nn =
  | S32
  | S64

type nonrec sx =
  | U
  | S

type nonrec iunop =
  | Clz
  | Ctz
  | Popcnt

type nonrec funop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

type nonrec ibinop =
  | Add
  | Sub
  | Mul
  | Div of sx
  | Rem of sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of sx
  | Rotl
  | Rotr

type nonrec fbinop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

type nonrec itestop = Eqz

type nonrec irelop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

type nonrec frelop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

type memarg =
  { offset : int
  ; align : int
  }

type nonrec limits =
  { min : int
  ; max : int option
  }

type mem = string option * limits

type final =
  | Final
  | No_final

module Make (M : sig
  type indice

  val pp_indice : Format.formatter -> indice -> unit

  type ('pt, 'rt) block_type

  val pp_block_type :
       (Format.formatter -> 'pt -> unit)
    -> (Format.formatter -> 'rt -> unit)
    -> Format.formatter
    -> ('pt, 'rt) block_type
    -> unit
end) =
struct
  (** Structure *)

  (** Types *)

  type indice = M.indice

  type heap_type =
    | Any_ht
    | None_ht
    | Eq_ht
    | I31_ht
    | Struct_ht
    | Array_ht
    | Func_ht
    | No_func_ht
    | Extern_ht
    | No_extern_ht
    | Def_ht of indice

  type nonrec ref_type = nullable * heap_type

  type nonrec val_type =
    | Num_type of num_type
    | Ref_type of ref_type

  type nonrec param = string option * val_type

  type nonrec param_type = param list

  type nonrec result_type = val_type list

  type nonrec func_type = param_type * result_type

  type block_type = (param_type, result_type) M.block_type

  type nonrec table_type = limits * ref_type

  type nonrec global_type = mut * val_type

  type nonrec extern_type =
    | Func of string option * func_type
    | Table of string option * table_type
    | Mem of string option * limits
    | Global of string option * global_type

  (** Instructions *)

  type instr =
    (* { desc : instr_desc
         ; loc : Lexing.position
         }

       and instr_desc =*)
    (* Numeric Instructions *)
    | I32_const of Int32.t
    | I64_const of Int64.t
    | F32_const of Float32.t
    | F64_const of Float64.t
    | I_unop of nn * iunop
    | F_unop of nn * funop
    | I_binop of nn * ibinop
    | F_binop of nn * fbinop
    | I_testop of nn * itestop
    | I_relop of nn * irelop
    | F_relop of nn * frelop
    | I_extend8_s of nn
    | I_extend16_s of nn
    | I64_extend32_s
    | I32_wrap_i64
    | I64_extend_i32 of sx
    | I_trunc_f of nn * nn * sx
    | I_trunc_sat_f of nn * nn * sx
    | F32_demote_f64
    | F64_promote_f32
    | F_convert_i of nn * nn * sx
    | I_reinterpret_f of nn * nn
    | F_reinterpret_i of nn * nn
    (* Reference instructions *)
    | Ref_null of heap_type
    | Ref_is_null
    | Ref_func of indice
    | Ref_as_non_null
    | Ref_cast of nullable * heap_type
    | Ref_test of nullable * heap_type
    | Ref_eq
    (* Parametric instructions *)
    | Drop
    | Select of val_type list option
    (* Variable instructions *)
    | Local_get of indice
    | Local_set of indice
    | Local_tee of indice
    | Global_get of indice
    | Global_set of indice
    (* Table instructions *)
    | Table_get of indice
    | Table_set of indice
    | Table_size of indice
    | Table_grow of indice
    | Table_fill of indice
    | Table_copy of indice * indice
    | Table_init of indice * indice
    | Elem_drop of indice
    (* Memory instructions *)
    | I_load of nn * memarg
    | F_load of nn * memarg
    | I_store of nn * memarg
    | F_store of nn * memarg
    | I_load8 of nn * sx * memarg
    | I_load16 of nn * sx * memarg
    | I64_load32 of sx * memarg
    | I_store8 of nn * memarg
    | I_store16 of nn * memarg
    | I64_store32 of memarg
    | Memory_size
    | Memory_grow
    | Memory_fill
    | Memory_copy
    | Memory_init of indice
    | Data_drop of indice
    (* Control instructions *)
    | Nop
    | Unreachable
    | Block of string option * block_type option * expr
    | Loop of string option * block_type option * expr
    | If_else of string option * block_type option * expr * expr
    | Br of indice
    | Br_if of indice
    | Br_table of indice array * indice
    | Br_on_cast of indice * nullable * heap_type
    | Br_on_cast_fail of indice * nullable * heap_type
    | Br_on_non_null of indice
    | Br_on_null of indice
    | Return
    | Return_call of indice
    | Return_call_indirect of indice * block_type
    | Return_call_ref of block_type
    | Call of indice
    | Call_indirect of indice * block_type
    | Call_ref of block_type
    (* Array instructions *)
    | Array_get of indice
    | Array_get_u of indice
    | Array_len
    | Array_new of indice
    | Array_new_data of indice * indice
    | Array_new_default of indice
    | Array_new_elem of indice * indice
    | Array_new_fixed of indice * int
    | Array_set of indice
    (* I31 *)
    | I31_get_u
    | I31_get_s
    | I31_new
    (* struct*)
    | Struct_get of indice * indice
    | Struct_get_s of indice * indice
    | Struct_new of indice
    | Struct_new_default of indice
    | Struct_set of indice * indice
    (* extern *)
    | Extern_externalize
    | Extern_internalize

  and expr = instr list

  (* TODO: func and expr should also be parametrised on block type:
     using block_type before simplify and directly an indice after *)
  type func =
    { type_f : block_type
    ; locals : param list
    ; body : expr
    ; id : string option
    }

  (* Tables & Memories *)

  type table = string option * table_type

  (* Modules *)

  type import_desc =
    | Import_func of string option * block_type
    | Import_table of string option * table_type
    | Import_mem of string option * limits
    | Import_global of string option * global_type

  type import =
    { modul : string
    ; name : string
    ; desc : import_desc
    }

  type export_desc =
    | Export_func of indice option
    | Export_table of indice option
    | Export_mem of indice option
    | Export_global of indice option

  type export =
    { name : string
    ; desc : export_desc
    }

  type storage_type =
    | Val_storage_t of val_type
    | Val_packed_t of packed_type

  type field_type = mut * storage_type

  type struct_field = string option * field_type list

  type struct_type = struct_field list

  type str_type =
    | Def_struct_t of struct_type
    | Def_array_t of field_type
    | Def_func_t of func_type

  type sub_type = final * indice list * str_type

  type type_def = string option * sub_type

  type rec_type = type_def list

  type const =
    | Const_I32 of Int32.t
    | Const_I64 of Int64.t
    | Const_F32 of Float32.t
    | Const_F64 of Float64.t
    | Const_null of heap_type
    | Const_host of int
    | Const_extern of int
    | Const_array
    | Const_eq
    | Const_i31
    | Const_struct

  module Const = struct
    type nonrec ibinop =
      | Add
      | Sub
      | Mul

    type instr =
      | I32_const of Int32.t
      | I64_const of Int64.t
      | F32_const of Float32.t
      | F64_const of Float64.t
      | Ref_null of heap_type
      | Ref_func of indice
      | Global_get of indice
      | I_binop of nn * ibinop
      | Array_new of indice
      | Array_new_default of indice
      | I31_new

    type expr = instr list
  end

  module Pp = struct
    let id fmt id = Format.fprintf fmt "$%s" id

    let indice = M.pp_indice

    let id_opt fmt = function None -> () | Some i -> id fmt i

    let print_nothing fmt () = Format.fprintf fmt ""

    let name fmt name = Format.pp_print_string fmt name

    let indice_opt fmt = function None -> () | Some i -> indice fmt i

    let num_type fmt = function
      | I32 -> Format.fprintf fmt "i32"
      | I64 -> Format.fprintf fmt "i64"
      | F32 -> Format.fprintf fmt "f32"
      | F64 -> Format.fprintf fmt "f64"

    let heap_type fmt = function
      | Any_ht -> Format.fprintf fmt "any"
      | None_ht -> Format.fprintf fmt "none"
      | Eq_ht -> Format.fprintf fmt "eq"
      | I31_ht -> Format.fprintf fmt "i31"
      | Struct_ht -> Format.fprintf fmt "struct"
      | Array_ht -> Format.fprintf fmt "array"
      | Func_ht -> Format.fprintf fmt "func"
      | No_func_ht -> Format.fprintf fmt "nofunc"
      | Extern_ht -> Format.fprintf fmt "extern"
      | No_extern_ht -> Format.fprintf fmt "noextern"
      | Def_ht i -> Format.fprintf fmt "%a" indice i

    let null fmt = function
      | No_null ->
        (* TODO: no notation to enforce nonnull ? *)
        Format.fprintf fmt ""
      | Null -> Format.fprintf fmt "null"

    let ref_type fmt (n, ht) =
      Format.fprintf fmt "(ref %a %a)" null n heap_type ht

    let val_type fmt = function
      | Num_type t -> num_type fmt t
      | Ref_type t -> ref_type fmt t

    let param fmt (id, vt) =
      Format.fprintf fmt "(param %a %a)" id_opt id val_type vt

    let param_type fmt params =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        param fmt params

    let result_ fmt vt = Format.fprintf fmt "(result %a)" val_type vt

    let result_type fmt results =
      Format.pp_print_list ~pp_sep:Format.pp_print_space result_ fmt results

    let func_type fmt (l, r) =
      Format.fprintf fmt "(func %a %a)" param_type l result_type r

    let block_type fmt bt = M.pp_block_type param_type result_type fmt bt

    let block_type_opt fmt = function
      | None -> ()
      | Some bt -> block_type fmt bt

    let limits fmt { min; max } =
      match max with
      | None -> Format.fprintf fmt "%d" min
      | Some max -> Format.fprintf fmt "%d %d" min max

    let table_type fmt (mt, rt) =
      Format.fprintf fmt "%a %a" limits mt ref_type rt

    let mut fmt = function Const -> () | Var -> Format.fprintf fmt "mut"

    let global_type fmt (m, vt) = Format.fprintf fmt "(%a %a)" mut m val_type vt

    let local fmt (id, t) =
      Format.fprintf fmt "(local %a %a)" id_opt id val_type t

    let locals fmt locals =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        local fmt locals

    let iunop fmt = function
      | Clz -> Format.fprintf fmt "clz"
      | Ctz -> Format.fprintf fmt "ctz"
      | Popcnt -> Format.fprintf fmt "popcnt"

    let funop fmt = function
      | Abs -> Format.fprintf fmt "abs"
      | Neg -> Format.fprintf fmt "neg"
      | Sqrt -> Format.fprintf fmt "sqrt"
      | Ceil -> Format.fprintf fmt "ceil"
      | Floor -> Format.fprintf fmt "floor"
      | Trunc -> Format.fprintf fmt "trunc"
      | Nearest -> Format.fprintf fmt "nearest"

    let sx fmt = function
      | U -> Format.fprintf fmt "u"
      | S -> Format.fprintf fmt "s"

    let ibinop fmt = function
      | (Add : ibinop) -> Format.fprintf fmt "add"
      | Sub -> Format.fprintf fmt "sub"
      | Mul -> Format.fprintf fmt "mul"
      | Div s -> Format.fprintf fmt "div_%a" sx s
      | Rem s -> Format.fprintf fmt "rem_%a" sx s
      | And -> Format.fprintf fmt "and"
      | Or -> Format.fprintf fmt "or"
      | Xor -> Format.fprintf fmt "xor"
      | Shl -> Format.fprintf fmt "shl"
      | Shr s -> Format.fprintf fmt "shr_%a" sx s
      | Rotl -> Format.fprintf fmt "rotl"
      | Rotr -> Format.fprintf fmt "rotr"

    let fbinop fmt = function
      | Add -> Format.fprintf fmt "add"
      | Sub -> Format.fprintf fmt "sub"
      | Mul -> Format.fprintf fmt "mul"
      | Div -> Format.fprintf fmt "div"
      | Min -> Format.fprintf fmt "min"
      | Max -> Format.fprintf fmt "max"
      | Copysign -> Format.fprintf fmt "copysign"

    let itestop fmt = function Eqz -> Format.fprintf fmt "eqz"

    let irelop fmt : irelop -> Unit.t = function
      | Eq -> Format.fprintf fmt "eq"
      | Ne -> Format.fprintf fmt "ne"
      | Lt s -> Format.fprintf fmt "lt_%a" sx s
      | Gt s -> Format.fprintf fmt "gt_%a" sx s
      | Le s -> Format.fprintf fmt "le_%a" sx s
      | Ge s -> Format.fprintf fmt "ge_%a" sx s

    let frelop fmt = function
      | Eq -> Format.fprintf fmt "eq"
      | Ne -> Format.fprintf fmt "ne"
      | Lt -> Format.fprintf fmt "lt"
      | Gt -> Format.fprintf fmt "gt"
      | Le -> Format.fprintf fmt "le"
      | Ge -> Format.fprintf fmt "ge"

    let f32 fmt f = Format.fprintf fmt "%s" (Float32.to_string f)

    let f64 fmt f = Format.fprintf fmt "%s" (Float64.to_string f)

    let nn fmt = function
      | S32 -> Format.fprintf fmt "32"
      | S64 -> Format.fprintf fmt "64"

    (* TODO: when offset is 0 then do not print anything, if offset is N (memargN) then print nothing ? *)
    let memarg fmt { offset; align } =
      if offset = 0 && align = 0 then ()
      else Format.fprintf fmt "offset=%d align=%d" offset align

    let rec instr fmt = function
      | I32_const i -> Format.fprintf fmt "i32.const %ld" i
      | I64_const i -> Format.fprintf fmt "i64.const %Ld" i
      | F32_const f -> Format.fprintf fmt "f32.const %a" f32 f
      | F64_const f -> Format.fprintf fmt "f64.const %a" f64 f
      | I_unop (n, op) -> Format.fprintf fmt "i%a.%a" nn n iunop op
      | F_unop (n, op) -> Format.fprintf fmt "f%a.%a" nn n funop op
      | I_binop (n, op) -> Format.fprintf fmt "i%a.%a" nn n ibinop op
      | F_binop (n, op) -> Format.fprintf fmt "f%a.%a" nn n fbinop op
      | I_testop (n, op) -> Format.fprintf fmt "i%a.%a" nn n itestop op
      | I_relop (n, op) -> Format.fprintf fmt "i%a.%a" nn n irelop op
      | F_relop (n, op) -> Format.fprintf fmt "f%a.%a" nn n frelop op
      | I_extend8_s n -> Format.fprintf fmt "i%a.extend8_s" nn n
      | I_extend16_s n -> Format.fprintf fmt "i%a.extend16_s" nn n
      | I64_extend32_s -> Format.fprintf fmt "i64.extend32_s"
      | I32_wrap_i64 -> Format.fprintf fmt "i32.wrap_i64"
      | I64_extend_i32 s -> Format.fprintf fmt "i64.extend_i32_%a" sx s
      | I_trunc_f (n, n', s) ->
        Format.fprintf fmt "i%a.trunc_f%a_%a" nn n nn n' sx s
      | I_trunc_sat_f (n, n', s) ->
        Format.fprintf fmt "i%a.trunc_sat_f%a_%a" nn n nn n' sx s
      | F32_demote_f64 -> Format.fprintf fmt "f32.demote_f64"
      | F64_promote_f32 -> Format.fprintf fmt "f64.promote_f32"
      | F_convert_i (n, n', s) ->
        Format.fprintf fmt "f%a.convert_i%a_%a" nn n nn n' sx s
      | I_reinterpret_f (n, n') ->
        Format.fprintf fmt "i%a.reinterpret_f%a" nn n nn n'
      | F_reinterpret_i (n, n') ->
        Format.fprintf fmt "f%a.reinterpret_i%a" nn n nn n'
      | Ref_null t -> Format.fprintf fmt "ref.null %a" heap_type t
      | Ref_is_null -> Format.fprintf fmt "ref.is_null"
      | Ref_func fid -> Format.fprintf fmt "ref.func %a" indice fid
      | Drop -> Format.fprintf fmt "drop"
      | Select vt -> begin
        match vt with
        | None -> Format.fprintf fmt "select"
        | Some vt -> Format.fprintf fmt "select (%a)" result_type vt
        (* TODO: are the parens needed ? *)
      end
      | Local_get id -> Format.fprintf fmt "local.get %a" indice id
      | Local_set id -> Format.fprintf fmt "local.set %a" indice id
      | Local_tee id -> Format.fprintf fmt "local.tee %a" indice id
      | Global_get id -> Format.fprintf fmt "global.get %a" indice id
      | Global_set id -> Format.fprintf fmt "global.set %a" indice id
      | Table_get id -> Format.fprintf fmt "table.get %a" indice id
      | Table_set id -> Format.fprintf fmt "table.set %a" indice id
      | Table_size id -> Format.fprintf fmt "table.size %a" indice id
      | Table_grow id -> Format.fprintf fmt "table.grow %a" indice id
      | Table_fill id -> Format.fprintf fmt "table.fill %a" indice id
      | Table_copy (id, id') ->
        Format.fprintf fmt "table.copy %a %a" indice id indice id'
      | Table_init (tid, eid) ->
        Format.fprintf fmt "table.init %a %a" indice tid indice eid
      | Elem_drop id -> Format.fprintf fmt "elem.drop %a" indice id
      | I_load (n, ma) -> Format.fprintf fmt "i%a.load %a" nn n memarg ma
      | F_load (n, ma) -> Format.fprintf fmt "f%a.load %a" nn n memarg ma
      | I_store (n, ma) -> Format.fprintf fmt "i%a.store %a" nn n memarg ma
      | F_store (n, ma) -> Format.fprintf fmt "f%a.store %a" nn n memarg ma
      | I_load8 (n, s, ma) ->
        Format.fprintf fmt "i%a.load8_%a %a" nn n sx s memarg ma
      | I_load16 (n, s, ma) ->
        Format.fprintf fmt "i%a.load16_%a %a" nn n sx s memarg ma
      | I64_load32 (s, ma) ->
        Format.fprintf fmt "i64.load32_%a %a" sx s memarg ma
      | I_store8 (n, ma) -> Format.fprintf fmt "i%a.store8 %a" nn n memarg ma
      | I_store16 (n, ma) -> Format.fprintf fmt "i%a.store16 %a" nn n memarg ma
      | I64_store32 ma -> Format.fprintf fmt "i64.store32 %a" memarg ma
      | Memory_size -> Format.fprintf fmt "memory.size"
      | Memory_grow -> Format.fprintf fmt "memory.grow"
      | Memory_fill -> Format.fprintf fmt "memory.fill"
      | Memory_copy -> Format.fprintf fmt "memory.copy"
      | Memory_init id -> Format.fprintf fmt "memory.init %a" indice id
      | Data_drop id -> Format.fprintf fmt "data.drop %a" indice id
      | Nop -> Format.fprintf fmt "nop"
      | Unreachable -> Format.fprintf fmt "unreachable"
      | Block (id, bt, _e) ->
        Format.fprintf fmt "block %a %a <expr>" id_opt id block_type_opt bt
      | Loop (id, bt, _expr) ->
        Format.fprintf fmt "loop %a %a <expr>" id_opt id block_type_opt bt
      | If_else (id, bt, e1, e2) ->
        Format.fprintf fmt
          "(if %a %a@\n\
          \  @[<v>(then@\n\
          \  @[<v>%a@]@\n\
           )@\n\
           (else@\n\
          \  @[<v>%a@]@\n\
           )@]@\n\
           )"
          id_opt id block_type_opt bt expr e1 expr e2
      | Br id -> Format.fprintf fmt "br %a" indice id
      | Br_if id -> Format.fprintf fmt "br_if %a" indice id
      | Br_table (ids, id) ->
        Format.fprintf fmt "br_table %a %a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
             indice )
          (Array.to_list ids) indice id
      | Return -> Format.fprintf fmt "return"
      | Return_call id -> Format.fprintf fmt "return_call %a" indice id
      | Return_call_indirect (tbl_id, ty_id) ->
        Format.fprintf fmt "return_call_indirect %a %a" indice tbl_id block_type
          ty_id
      | Return_call_ref ty_id ->
        Format.fprintf fmt "return_call_ref %a" block_type ty_id
      | Call id -> Format.fprintf fmt "call %a" indice id
      | Call_indirect (tbl_id, ty_id) ->
        Format.fprintf fmt "call_indirect %a %a" indice tbl_id block_type ty_id
      | Call_ref ty_id -> Format.fprintf fmt "call_ref %a" block_type ty_id
      | Array_new id -> Format.fprintf fmt "array.new %a" indice id
      | Array_new_data (id1, id2) ->
        Format.fprintf fmt "array.new_data %a %a" indice id1 indice id2
      | Array_new_default id ->
        Format.fprintf fmt "array.new_default %a" indice id
      | Array_new_elem (id1, id2) ->
        Format.fprintf fmt "array.new_elem %a %a" indice id1 indice id2
      | Array_new_fixed (id, i) ->
        Format.fprintf fmt "array.new_fixed %a %d" indice id i
      | Array_get id -> Format.fprintf fmt "array.get %a" indice id
      | Array_get_u id -> Format.fprintf fmt "array.get_u %a" indice id
      | Array_set id -> Format.fprintf fmt "array.set %a" indice id
      | Array_len -> Format.fprintf fmt "array.len"
      | I31_new -> Format.fprintf fmt "i31.new"
      | I31_get_s -> Format.fprintf fmt "i31.get_s"
      | I31_get_u -> Format.fprintf fmt "i31.get_u"
      | Struct_get (i1, i2) ->
        Format.fprintf fmt "struct.get %a %a" indice i1 indice i2
      | Struct_get_s (i1, i2) ->
        Format.fprintf fmt "struct.get_s %a %a" indice i1 indice i2
      | Struct_new i -> Format.fprintf fmt "struct.new %a" indice i
      | Struct_new_default i ->
        Format.fprintf fmt "struct.new_default %a" indice i
      | Struct_set (i1, i2) ->
        Format.fprintf fmt "struct.set %a %a" indice i1 indice i2
      | Extern_externalize -> Format.fprintf fmt "extern.externalize"
      | Extern_internalize -> Format.fprintf fmt "extern.internalize"
      | Ref_as_non_null -> Format.fprintf fmt "ref.as_non_null"
      | Ref_cast (n, t) ->
        Format.fprintf fmt "ref.cast %a %a" null n heap_type t
      | Ref_test (n, t) ->
        Format.fprintf fmt "ref.test %a %a" null n heap_type t
      | Br_on_non_null id -> Format.fprintf fmt "br_on_non_null %a" indice id
      | Br_on_null id -> Format.fprintf fmt "br_on_null %a" indice id
      | Br_on_cast (id, n, t) ->
        Format.fprintf fmt "br_on_cast %a %a %a" indice id null n heap_type t
      | Br_on_cast_fail (id, n, t) ->
        Format.fprintf fmt "br_on_cast_fail %a %a %a" indice id null n heap_type
          t
      | Ref_eq -> Format.fprintf fmt "ref.eq"

    and expr fmt instrs =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
        instr fmt instrs

    let func fmt (f : func) =
      (* TODO: typeuse ? *)
      Format.fprintf fmt "(func %a %a %a@\n  @[<v>%a@]@\n)" id_opt f.id
        block_type f.type_f locals f.locals expr f.body

    let funcs fmt (funcs : func list) =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
        func fmt funcs

    let start fmt start = Format.fprintf fmt "(start %a)" indice start

    let mem fmt (id, ty) =
      Format.fprintf fmt "(memory %a %a)" id_opt id limits ty

    let table fmt (id, ty) =
      Format.fprintf fmt "(table %a %a)" id_opt id table_type ty

    let import_desc fmt : import_desc -> Unit.t = function
      | Import_func (id, t) ->
        Format.fprintf fmt "(func %a %a)" id_opt id block_type t
      | Import_table (id, t) ->
        Format.fprintf fmt "(table %a %a)" id_opt id table_type t
      | Import_mem (id, t) ->
        Format.fprintf fmt "(memory %a %a)" id_opt id limits t
      | Import_global (id, t) ->
        Format.fprintf fmt "(global %a %a)" id_opt id global_type t

    let import fmt i =
      Format.fprintf fmt {|(import "%a" "%a" %a)|} name i.modul name i.name
        import_desc i.desc

    let packed_type fmt = function
      | I8 -> Format.fprintf fmt "i8"
      | I16 -> Format.fprintf fmt "i16"

    let storage_type fmt = function
      | Val_storage_t t -> val_type fmt t
      | Val_packed_t t -> packed_type fmt t

    let field_type fmt (m, t) = Format.fprintf fmt "%a %a" mut m storage_type t

    let fields fmt =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        field_type fmt

    let struct_field fmt ((n : string option), f) =
      Format.fprintf fmt "@\n  @[<v>(field %a%a)@]" id_opt n fields f

    let struct_type fmt =
      Format.fprintf fmt "(struct %a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           struct_field )

    let array_type fmt = Format.fprintf fmt "(array %a)" field_type

    let str_type fmt = function
      | Def_struct_t t -> struct_type fmt t
      | Def_array_t t -> array_type fmt t
      | Def_func_t t -> func_type fmt t

    let indices fmt ids =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        indice fmt ids

    let final fmt = function
      | Final -> Format.fprintf fmt "final"
      | No_final -> Format.fprintf fmt "no_final"

    let sub_type fmt (f, ids, t) =
      Format.fprintf fmt "(sub %a %a %a)" final f indices ids str_type t

    let type_def fmt (id, t) =
      Format.fprintf fmt "@\n  @[<v>(type %a %a)@]" id_opt id sub_type t

    let typ fmt l =
      (* TODO: special case for empty and singleton case to avoid a big rec printing *)
      Format.fprintf fmt "(rec %a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           type_def )
        l

    module Pp_const = struct
      let ibinop fmt : Const.ibinop -> Unit.t = function
        | Add -> Format.fprintf fmt "add"
        | Sub -> Format.fprintf fmt "sub"
        | Mul -> Format.fprintf fmt "mul"

      let instr fmt = function
        | Const.I32_const i -> Format.fprintf fmt "i32.const %ld" i
        | I64_const i -> Format.fprintf fmt "i64.const %Ld" i
        | F32_const f -> Format.fprintf fmt "f32.const %a" f32 f
        | F64_const f -> Format.fprintf fmt "f64.const %a" f64 f
        | I_binop (n, op) -> Format.fprintf fmt "i%a.%a" nn n ibinop op
        | Global_get id -> Format.fprintf fmt "global.get %a" indice id
        | Ref_null t -> Format.fprintf fmt "ref.null %a" heap_type t
        | Ref_func fid -> Format.fprintf fmt "ref.func %a" indice fid
        | Array_new _ -> Format.fprintf fmt "array.new"
        | Array_new_default _ -> Format.fprintf fmt "array.new_default"
        | I31_new -> Format.fprintf fmt "i31.new"

      let expr fmt instrs =
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
          instr fmt instrs
    end
  end
end

module Symbolic = struct
  module Arg = struct
    type indice =
      | Symbolic of string
      | Raw of int

    let pp_indice fmt = function
      | Raw u -> Format.pp_print_int fmt u
      (* TODO: this is the id function that should be factored out *)
      | Symbolic i -> Format.fprintf fmt "$%s" i

    type ('pt, 'rt) block_type =
      | Bt_ind of indice
      | Bt_raw of (indice option * ('pt * 'rt))
    (* the indice option is the optional typeuse, if it's some it must be equal to the func_type *)

    let pp_block_type pp_param_type pp_result_type fmt v =
      match v with
      | Bt_ind ind -> Format.fprintf fmt "(type %a)" pp_indice ind
      | Bt_raw (_ind, (pt, rt)) ->
        Format.fprintf fmt "%a %a" pp_param_type pt pp_result_type rt
  end

  include Make (Arg)

  let symbolic v = Arg.Symbolic v

  let raw v = Arg.Raw v

  let bt_ind i = Arg.Bt_ind i

  let bt_raw i t = Arg.Bt_raw (i, t)

  type global =
    { typ : global_type
    ; init : expr
    ; id : string option
    }

  type data_mode =
    | Data_passive
    | Data_active of indice option * expr

  type data =
    { id : string option
    ; init : string
    ; mode : data_mode
    }

  type elem_mode =
    | Elem_passive
    | Elem_active of indice option * expr
    | Elem_declarative

  type elem =
    { id : string option
    ; typ : ref_type
    ; init : expr list
    ; mode : elem_mode
    }

  type module_field =
    | MType of rec_type
    | MGlobal of global
    | MTable of table
    | MMem of mem
    | MFunc of func
    | MElem of elem
    | MData of data
    | MStart of indice
    | MImport of import
    | MExport of export

  type modul =
    { id : string option
    ; fields : module_field list
    }

  type action =
    | Invoke of string option * string * const list
    | Get of string option * string

  type result_const =
    | Literal of const
    | Nan_canon of nn
    | Nan_arith of nn

  type result =
    | Result_const of result_const
    | Result_extern_ref
    | Result_func_ref

  type assert_ =
    | Assert_return of action * result list
    | Assert_trap of action * string
    | Assert_trap_module of modul * string
    | Assert_malformed of modul * string
    | Assert_malformed_quote of string list * string
    | Assert_malformed_binary of string list * string
    | Assert_invalid of modul * string
    | Assert_invalid_quote of string list * string
    | Assert_invalid_binary of string list * string
    | Assert_exhaustion of action * string
    | Assert_unlinkable of modul * string

  type cmd =
    | Module of modul
    | Assert of assert_
    | Register of string * string option
    | Action of action

  type script = cmd list

  module Pp = struct
    include Pp

    let global fmt (g : global) =
      Format.fprintf fmt "(global %a %a %a)" id_opt g.id global_type g.typ expr
        g.init

    let symb_indice_opt fmt = function None -> () | Some i -> indice fmt i

    let id_opt fmt = function None -> () | Some i -> id fmt i

    let export_desc fmt = function
      | Export_func id -> Format.fprintf fmt "(func %a)" symb_indice_opt id
      | Export_table id -> Format.fprintf fmt "(table %a)" symb_indice_opt id
      | Export_mem id -> Format.fprintf fmt "(memory %a)" symb_indice_opt id
      | Export_global id -> Format.fprintf fmt "(global %a)" symb_indice_opt id

    let export fmt e =
      Format.fprintf fmt "(export %a %a)" name e.name export_desc e.desc

    let elem_mode fmt = function
      | Elem_passive -> ()
      | Elem_declarative -> Format.fprintf fmt "declare"
      | Elem_active (i, e) -> (
        match i with
        | None -> Format.fprintf fmt "(offset %a)" expr e
        | Some i -> Format.fprintf fmt "(table %a) (offset %a)" indice i expr e
        )

    let elemexpr fmt e = Format.fprintf fmt "(item %a)" expr e

    let data_mode fmt = function
      | Data_passive -> ()
      | Data_active (i, e) ->
        Format.fprintf fmt "(%a %a)" symb_indice_opt i expr e

    let data fmt (d : data) =
      Format.fprintf fmt {|(data %a %S)|} data_mode d.mode d.init

    let elem fmt (e : elem) =
      Format.fprintf fmt "@[<hov 2>(elem %a %a %a %a)@]" id_opt e.id elem_mode
        e.mode ref_type e.typ
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           elemexpr )
        e.init

    let module_field fmt = function
      | MType t -> typ fmt t
      | MGlobal g -> global fmt g
      | MTable t -> table fmt t
      | MMem m -> mem fmt m
      | MFunc f -> func fmt f
      | MElem e -> elem fmt e
      | MData d -> data fmt d
      | MStart s -> start fmt s
      | MImport i -> import fmt i
      | MExport e -> export fmt e

    let modul fmt m =
      Format.fprintf fmt "(module %a@\n  @[<v>%a@]@\n)" id_opt m.id
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           module_field )
        m.fields

    let register fmt (s, _name) = Format.fprintf fmt "(register %s)" s

    let const fmt = function
      | Const_I32 i -> Format.fprintf fmt "i32.const %ld" i
      | Const_I64 i -> Format.fprintf fmt "i64.const %Ld" i
      | Const_F32 f -> Format.fprintf fmt "f32.const %a" f32 f
      | Const_F64 f -> Format.fprintf fmt "f64.const %a" f64 f
      | Const_null rt -> Format.fprintf fmt "ref.null %a" heap_type rt
      | Const_host i -> Format.fprintf fmt "ref.host %d" i
      | Const_extern i -> Format.fprintf fmt "ref.extern %d" i
      | Const_array -> Format.fprintf fmt "ref.array"
      | Const_eq -> Format.fprintf fmt "ref.eq"
      | Const_i31 -> Format.fprintf fmt "ref.i31"
      | Const_struct -> Format.fprintf fmt "ref.struct"

    let consts fmt c =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        (fun fmt c -> Format.fprintf fmt "(%a)" const c)
        fmt c

    let action fmt = function
      | Invoke (mod_name, name, c) ->
        Format.fprintf fmt "(invoke %a %s %a)" id_opt mod_name name consts c
      | Get _ -> Format.fprintf fmt "<action_get TODO>"

    let result_const fmt = function
      | Literal c -> const fmt c
      | Nan_canon n -> Format.fprintf fmt "float%a.const nan:canonical" nn n
      | Nan_arith n -> Format.fprintf fmt "float%a.const nan:arithmetic" nn n

    let result fmt = function
      | Result_const c -> Format.fprintf fmt "(%a)" result_const c
      | _ -> Log.err "not yet implemented"

    let result_bis fmt = function
      | Result_const c -> Format.fprintf fmt "%a" result_const c
      | _ -> Format.fprintf fmt "<results TODO>"

    let results fmt r =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
        result_bis fmt r

    let strings fmt l =
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           Format.pp_print_string )
        l

    let assert_ fmt = function
      | Assert_return (a, l) ->
        Format.fprintf fmt "(assert_return %a %a)" action a results l
      | Assert_exhaustion (a, msg) ->
        Format.fprintf fmt "(assert_exhaustion %a %s)" action a msg
      | Assert_trap (a, f) ->
        Format.fprintf fmt {|(assert_trap %a "%s")|} action a f
      | Assert_trap_module (m, f) ->
        Format.fprintf fmt {|(assert_trap_module %a "%s")|} modul m f
      | Assert_invalid (m, msg) ->
        Format.fprintf fmt "(assert_invalid@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)"
          modul m msg
      | Assert_unlinkable (m, msg) ->
        Format.fprintf fmt "(assert_unlinkable@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)"
          modul m msg
      | Assert_malformed (m, msg) ->
        Format.fprintf fmt "(assert_malformed@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)"
          modul m msg
      | Assert_malformed_quote (ls, msg) ->
        Format.fprintf fmt
          "(assert_malformed_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls
          msg
      | Assert_invalid_quote (ls, msg) ->
        Format.fprintf fmt
          "(assert_invalid_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls msg
      | Assert_malformed_binary (ls, msg) ->
        Format.fprintf fmt
          "(assert_malformed_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls
          msg
      | Assert_invalid_binary (ls, msg) ->
        Format.fprintf fmt
          "(assert_invalid_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls
          msg

    let cmd fmt = function
      | Module m -> modul fmt m
      | Assert a -> assert_ fmt a
      | Register (s, name) -> register fmt (s, name)
      | Action _a -> Format.fprintf fmt "<action>"

    let file fmt l =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
        cmd fmt l
  end
end

module StringMap = Map.Make (String)

(** int indexed values *)
type 'a indexed =
  { index : int
  ; value : 'a
  }

let has_index idx { index; _ } = idx = index

(** the types of imported values *)
type 'a imp =
  { modul : string
  ; name : string
  ; assigned_name : string option
  ; desc : 'a
  }

type ('a, 'b) runtime =
  | Local of 'a
  | Imported of 'b imp

(** named values (fields) *)
module Named : sig
  type 'a t =
    { values : 'a indexed list
    ; named : int StringMap.t
    }

  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (int -> 'a -> unit) -> 'a t -> unit

  val map : ('a indexed -> 'b indexed) -> 'a t -> 'b t
end = struct
  type 'a t =
    { values : 'a indexed list
    ; named : int StringMap.t
    }

  let fold f v acc =
    List.fold_left (fun acc v -> f v.index v.value acc) acc v.values

  let iter f v = List.iter (fun v -> f v.index v.value) v.values

  let map f v =
    let values = List.map f v.values in
    { v with values }
end

module Simplified = struct
  module Arg = struct
    type indice = int

    let pp_indice fmt u = Format.pp_print_int fmt u

    type ('pt, 'rt) block_type = 'pt * 'rt

    let pp_block_type pp_param_type pp_result_type fmt (pt, rt) =
      Format.fprintf fmt "%a %a" pp_param_type pt pp_result_type rt
  end

  include Make (Arg)

  (** named export *)
  type export =
    { name : string
    ; id : int
    }

  (** named exports of a module *)
  type exports =
    { global : export list
    ; mem : export list
    ; table : export list
    ; func : export list
    }

  type global =
    { typ : global_type
    ; init : Const.expr
    ; id : string option
    }

  type data_mode =
    | Data_passive
    | Data_active of indice option * Const.expr

  type data =
    { id : string option
    ; init : string
    ; mode : data_mode
    }

  type elem_mode =
    | Elem_passive
    | Elem_active of indice option * Const.expr
    | Elem_declarative

  type elem =
    { id : string option
    ; typ : ref_type
    ; init : Const.expr list
    ; mode : elem_mode
    }

  type modul =
    { id : string option
    ; global : (global, global_type) runtime Named.t
    ; table : (table, table_type) runtime Named.t
    ; mem : (mem, limits) runtime Named.t
    ; func : (func, func_type) runtime Named.t
    ; elem : elem Named.t
    ; data : data Named.t
    ; exports : exports
    ; start : int option
    }

  module Pp = struct
    include Pp

    let id fmt = Option.iter (fun id -> Format.fprintf fmt " $%s" id)

    let func fmt (f : (func, _) runtime) =
      match f with
      | Local f -> Format.fprintf fmt "%a" func f
      | Imported { modul; name; _ } -> Format.fprintf fmt "%s.%s" modul name

    let indexed f fmt indexed = Format.fprintf fmt "%a" f indexed.value

    let lst f fmt l =
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") f)
        fmt (List.rev l)

    let funcs fmt (funcs : _ runtime Named.t) =
      lst (indexed func) fmt funcs.values

    let export fmt (export : export) =
      Format.fprintf fmt "%s: %a" export.name indice export.id

    let start fmt = function
      | None -> ()
      | Some ind -> Format.fprintf fmt "(start %a)" indice ind

    let modul fmt (m : modul) : unit =
      Format.fprintf fmt "(module%a@\n  @[%a@\n%a@]@\n)" id m.id funcs m.func
        start m.start
  end
end
