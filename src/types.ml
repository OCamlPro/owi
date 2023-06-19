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
      | Block (id, bt, e) ->
        Format.fprintf fmt "(block %a %a@\n  @[<v>%a@])" id_opt id
          block_type_opt bt expr e
      | Loop (id, bt, e) ->
        Format.fprintf fmt "(loop %a %a@\n  @[<v>%a@])" id_opt id block_type_opt
          bt expr e
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
        Format.fprintf fmt "ref.cast (ref %a %a)" null n heap_type t
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

    let field_type fmt (m, t) =
      match m with
      | Const -> Format.fprintf fmt " %a" storage_type t
      | Var -> Format.fprintf fmt "(%a %a)" mut m storage_type t

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
