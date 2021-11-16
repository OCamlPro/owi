open Types

let print_nothing fmt () = Format.fprintf fmt ""

let id fmt id =
  (* TODO: stop being dumb :-) *)
  Format.fprintf fmt "$%s" id

let id_opt fmt = function
  | None -> ()
  | Some i -> id fmt i

let u32 fmt u = Unsigned.UInt32.pp fmt u

let i32 fmt i = Signed.Int32.pp fmt i

let i64 fmt i = Signed.Int64.pp fmt i

let f32 fmt f = Format.fprintf fmt "%f" f

let f64 fmt f = Format.fprintf fmt "%f" f

let char fmt c =
  (* TODO: ? *)
  Format.fprintf fmt "%c" (Uchar.to_char c)

let name fmt name = Format.pp_print_string fmt name

let indice fmt = function
  | Raw u -> u32 fmt u
  | Symbolic i -> id fmt i

let local_idx = indice

let func_idx = indice

let type_idx = indice

let global_idx = indice

let table_idx = indice

let elem_idx = indice

let data_idx = indice

let label_idx = indice

let mem_idx = indice

let num_type fmt : num_type -> Unit.t = function
  | I32 -> Format.fprintf fmt "i32"
  | I64 -> Format.fprintf fmt "i64"
  | F32 -> Format.fprintf fmt "f32"
  | F64 -> Format.fprintf fmt "f64"

let ref_type fmt = function
  | Func_ref -> Format.fprintf fmt "funcref"
  | Extern_ref -> Format.fprintf fmt "externref"

let val_type fmt = function
  | Num_type t -> num_type fmt t
  | Ref_type t -> ref_type fmt t

(* TODO: ? *)
let block_type fmt = function
  | Type_idx id -> Format.fprintf fmt "(result %a)" type_idx id
  | Val_type None -> ()
  | Val_type (Some t) -> Format.fprintf fmt "(result %a)" val_type t

let param fmt vt = Format.fprintf fmt "(param %a)" val_type vt

let param_type fmt params =
  Format.pp_print_list ~pp_sep:Format.pp_print_space param fmt params

let result_ fmt vt = Format.fprintf fmt "(result %a)" val_type vt

let result_type fmt results =
  Format.pp_print_list ~pp_sep:Format.pp_print_space result_ fmt results

let func_type fmt (l, r) =
  Format.fprintf fmt "(func %a %a)" param_type l result_type r

let limits fmt { min; max } =
  match max with
  | None -> Format.fprintf fmt "%a" u32 min
  | Some max -> Format.fprintf fmt "%a %a" u32 min u32 max

let mem_type fmt t = limits fmt t

let table_type fmt (mt, rt) = Format.fprintf fmt "%a %a" mem_type mt ref_type rt

let mut fmt = function
  | Const -> ()
  | Var -> Format.fprintf fmt "mut"

let global_type fmt (m, vt) = Format.fprintf fmt "(%a %a)" mut m val_type vt

let local fmt (id, t) = Format.fprintf fmt "(local %a %a)" id_opt id val_type t

let locals fmt locals =
  Format.pp_print_list ~pp_sep:Format.pp_print_space local fmt locals

let nn fmt = function
  | S32 -> Format.fprintf fmt "32"
  | S64 -> Format.fprintf fmt "64"

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

let ibinop fmt : Types.ibinop -> Unit.t = function
  | Add -> Format.fprintf fmt "add"
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

let fbinop fmt : Types.fbinop -> Unit.t = function
  | Add -> Format.fprintf fmt "add"
  | Sub -> Format.fprintf fmt "sub"
  | Mul -> Format.fprintf fmt "mul"
  | Div -> Format.fprintf fmt "div"
  | Min -> Format.fprintf fmt "min"
  | Max -> Format.fprintf fmt "max"
  | Copysign -> Format.fprintf fmt "copysign"

let itestop fmt = function
  | Eqz -> Format.fprintf fmt "eqz"

let irelop fmt : irelop -> Unit.t = function
  | Eq -> Format.fprintf fmt "eq"
  | Ne -> Format.fprintf fmt "ne"
  | Lt s -> Format.fprintf fmt "lt_%a" sx s
  | Gt s -> Format.fprintf fmt "gt_%a" sx s
  | Le s -> Format.fprintf fmt "le_%a" sx s
  | Ge s -> Format.fprintf fmt "gs_%a" sx s

let frelop fmt = function
  | Eq -> Format.fprintf fmt "eq"
  | Ne -> Format.fprintf fmt "ne"
  | Lt -> Format.fprintf fmt "lt"
  | Gt -> Format.fprintf fmt "gt"
  | Le -> Format.fprintf fmt "le"
  | Ge -> Format.fprintf fmt "ge"

(* TODO: when offset is 0 then do not print anything, if offset is N (memargN) then print nothing ? *)
let memarg fmt { offset; align } =
  Format.fprintf fmt "offset=%a align=%a" u32 offset u32 align

let instr fmt = function
  | I32_const i -> Format.fprintf fmt "i32.const %a" i32 i
  | I64_const i -> Format.fprintf fmt "i64.const %a" i64 i
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
  | Ref_null t -> Format.fprintf fmt "ref.null %a" ref_type t
  | Ref_is_null -> Format.fprintf fmt "ref.is_null"
  | Ref_func fid -> Format.fprintf fmt "ref.func %a" func_idx fid
  | Drop -> Format.fprintf fmt "drop"
  | Select vt -> begin
    match vt with
    | None -> Format.fprintf fmt "select"
    | Some vt ->
      Format.fprintf fmt "select (%a)" result_type vt
      (* TODO: are the parens needed ? *)
  end
  | Local_get id -> Format.fprintf fmt "local.get %a" local_idx id
  | Local_set id -> Format.fprintf fmt "local.set %a" local_idx id
  | Local_tee id -> Format.fprintf fmt "local.tee %a" local_idx id
  | Global_get id -> Format.fprintf fmt "global.get %a" global_idx id
  | Global_set id -> Format.fprintf fmt "global.set %a" global_idx id
  | Table_get id -> Format.fprintf fmt "table.get %a" table_idx id
  | Table_set id -> Format.fprintf fmt "table.set %a" table_idx id
  | Table_size id -> Format.fprintf fmt "table.size %a" table_idx id
  | Table_grow id -> Format.fprintf fmt "table.grow %a" table_idx id
  | Table_fill id -> Format.fprintf fmt "table.fill %a" table_idx id
  | Table_copy (id, id') ->
    Format.fprintf fmt "table.copy %a %a" table_idx id table_idx id'
  | Table_init (tid, eid) ->
    Format.fprintf fmt "table.init %a %a" table_idx tid elem_idx eid
  | Elem_drop id -> Format.fprintf fmt "elem.drop %a" elem_idx id
  | I_load (n, ma) ->
    Format.fprintf fmt "i%a.load %a" nn n memarg ma
  | F_load (n, ma) ->
    Format.fprintf fmt "f%a.load %a" nn n memarg ma
  | I_store (n, ma) ->
    Format.fprintf fmt "i%a.load %a" nn n memarg ma
  | F_store (n, ma) ->
    Format.fprintf fmt "f%a.load %a" nn n memarg ma
  | I_load8 (n, s, ma) ->
    Format.fprintf fmt "i%a.load8_%a %a" nn n sx s memarg ma
  | I_load16 (n, s, ma) ->
    Format.fprintf fmt "i%a.load16_%a %a" nn n sx s memarg ma
  | I64_load32 (s, ma) ->
    Format.fprintf fmt "i64.load32_%a %a" sx s memarg ma
  | I_store8 (n, ma) ->
    Format.fprintf fmt "i%a.store8 %a" nn n memarg ma
  | I_store16 (n, ma) ->
    Format.fprintf fmt "i%a.store16 %a" nn n memarg ma
  | I64_store32 ma ->
    Format.fprintf fmt "i64.store32 %a" memarg ma
  | Memory_size -> Format.fprintf fmt "memory.size"
  | Memory_grow -> Format.fprintf fmt "memory.grow"
  | Memory_fill -> Format.fprintf fmt "memory.fill"
  | Memory_copy -> Format.fprintf fmt "memory.copy"
  | Memory_init id -> Format.fprintf fmt "memory.init %a" data_idx id
  | Data_drop id -> Format.fprintf fmt "data.drop %a" data_idx id
  | Nop -> Format.fprintf fmt "nop"
  | Unreachable -> Format.fprintf fmt "unreachable"
  | Block (_ty, _expr) -> Format.fprintf fmt "<block>"
  | Loop (_ty, _expr) -> Format.fprintf fmt "<loop>"
  | If_else (_ty, _expr, _expr') ->
    Format.fprintf fmt "<if>"
  | Br id -> Format.fprintf fmt "br %a" label_idx id
  | Br_if id -> Format.fprintf fmt "br_if %a" label_idx id
  | Br_table (_ids, _id) -> Format.fprintf fmt "<br_table>"
  | Return -> Format.fprintf fmt "return"
  | Call id -> Format.fprintf fmt "call %a" func_idx id
  | Call_indirect (_tbl_id, _ty_id) -> Format.fprintf fmt "<call_indirect>"

let body fmt instrs =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline instr fmt instrs

let func fmt (f : func) =
  (* TODO: typeuse ? *)
  Format.fprintf fmt "(func %a %a@.%a)" id_opt f.id locals f.locals body f.body

let expr fmt _e =
  Format.fprintf fmt "<expr>"

let datas fmt _datas =
  Format.fprintf fmt "<datas>"

let elems fmt _elems =
  Format.fprintf fmt "<elems>"

let start fmt = function
  | None -> ()
  | Some start -> Format.fprintf fmt "(start %a)" func_idx start

let export_desc fmt = function
  | Export_func id -> Format.fprintf fmt "(func %a)" func_idx id
  | Export_table id -> Format.fprintf fmt "(table %a)" table_idx id
  | Export_mem id -> Format.fprintf fmt "(memory %a)" mem_idx id
  | Export_global id -> Format.fprintf fmt "(global %a)" global_idx id

let export fmt e =
  Format.fprintf fmt "(export %a %a)" name e.name export_desc e.desc

let exports fmt exports =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline export fmt exports

let global fmt (g : global) =
  Format.fprintf fmt "(global %a %a %a)" id_opt g.id global_type g.type_ expr
    g.init

let globals fmt globals =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline global fmt globals

let mem fmt (id, ty) = Format.fprintf fmt "(memory %a %a)" id_opt id mem_type ty

let mems fmt mems =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline mem fmt mems

let table fmt (id, ty) =
  Format.fprintf fmt "(table %a %a)" id_opt id table_type ty

let tables fmt tables =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline table fmt tables

let funcs fmt (funcs : func list) =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline func fmt funcs

let import_desc fmt : import_desc -> Unit.t = function
  (* TODO: in Func case, check what is the "typeuse" *)
  | Import_func (_id, _tidx) -> assert false
    (* Format.fprintf fmt "(func %a %a)" id_opt id type_idx tidx *)
  | Import_table (id, t) ->
    Format.fprintf fmt "(table %a %a)" id_opt id table_type t
  | Import_mem (id, t) ->
    Format.fprintf fmt "(memory %a %a)" id_opt id mem_type t
  | Import_global (id, t) ->
    Format.fprintf fmt "(global %a %a)" id_opt id global_type t

let import fmt i =
  Format.fprintf fmt "(import %a %a %a)" name i.module_ name i.name import_desc
    i.desc

let imports fmt imports =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline import fmt imports

let type_ fmt (i, ft) = Format.fprintf fmt "(type %a %a)" id_opt i func_type ft

let types fmt types =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline type_ fmt types

let module_fields fmt m =
  Format.fprintf fmt "%a@.%a@.%a@.%a@.%a@.%a@.%a@.%a@.%a@.%a@." types m.types imports
    m.imports funcs m.funcs tables m.tables mems m.mems globals m.globals
    exports m.exports start m.start elems m.elems datas m.datas

let module_ fmt m =
  Format.fprintf fmt "(module %a@.%a)" id_opt m.id module_fields m

let register fmt s =
  Format.fprintf fmt "(register %s)" s

let stanza fmt = function
  | Module m -> module_ fmt m
  | Assert _ -> Format.fprintf fmt "<assert>"
  | Register s -> register fmt s
  | Invoke (_n, _es) -> Format.fprintf fmt "<invoke>"


let file fmt l =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline stanza fmt l
