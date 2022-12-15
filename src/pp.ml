(** Module to pretty print the various types such as text module or simplified
    module. *)

open Types

module type Arg = sig
  type indice

  type bt

  val indice : Format.formatter -> indice -> unit

  val bt : Format.formatter -> bt -> unit
end

let id fmt id = Format.fprintf fmt "$%s" id

module Shared = struct
  let id_opt fmt = function None -> () | Some i -> id fmt i

  let num_type fmt : num_type -> Unit.t = function
    | I32 -> Format.fprintf fmt "i32"
    | I64 -> Format.fprintf fmt "i64"
    | F32 -> Format.fprintf fmt "f32"
    | F64 -> Format.fprintf fmt "f64"

  let ref_type fmt = function
    | Func_ref -> Format.fprintf fmt "funcref"
    | Extern_ref -> Format.fprintf fmt "extern"

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

  let f32 fmt f = Format.fprintf fmt "%s" (Float32.to_string f)

  let f64 fmt f = Format.fprintf fmt "%s" (Float64.to_string f)

  let nn fmt = function
    | S32 -> Format.fprintf fmt "32"
    | S64 -> Format.fprintf fmt "64"
end

module Symbolic_indice :
  Arg with type indice = Types.indice and type bt = indice block_type = struct
  type indice = Types.indice

  type bt = indice block_type

  let indice fmt = function
    | Raw u -> Format.pp_print_int fmt u
    | Symbolic i -> id fmt i

  let bt fmt = function
    | Bt_ind ind -> Format.fprintf fmt "%a" indice ind
    | Bt_raw (_type_use, (l, r)) ->
      Format.fprintf fmt "%a %a" Shared.param_type l Shared.result_type r
end

module Simplified_indice : Arg with type indice = int and type bt = func_type =
struct
  type indice = int

  type bt = func_type

  let indice fmt u = Format.pp_print_int fmt u

  let bt fmt (l, r) =
    Format.fprintf fmt "%a %a" Shared.param_type l Shared.result_type r
end

module Const = struct
  let ibinop fmt : Types.Const.ibinop -> Unit.t = function
    | Add -> Format.fprintf fmt "add"
    | Sub -> Format.fprintf fmt "sub"
    | Mul -> Format.fprintf fmt "mul"

  let instr fmt (const : Const.instr) =
    match const with
    | I32_const i -> Format.fprintf fmt "i32.const %ld" i
    | I64_const i -> Format.fprintf fmt "i64.const %Ld" i
    | F32_const f -> Format.fprintf fmt "f32.const %a" Shared.f32 f
    | F64_const f -> Format.fprintf fmt "f64.const %a" Shared.f64 f
    | I_binop (n, op) -> Format.fprintf fmt "i%a.%a" Shared.nn n ibinop op
    | Global_get id ->
      Format.fprintf fmt "global.get %a" Simplified_indice.indice id
    | Ref_null t -> Format.fprintf fmt "ref.null %a" Shared.ref_type t
    | Ref_func fid ->
      Format.fprintf fmt "ref.func %a" Simplified_indice.indice fid

  let expr fmt instrs =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      instr fmt instrs
end

module Make_Expr (Arg : Arg) = struct
  include Shared

  let print_nothing fmt () = Format.fprintf fmt ""

  let name fmt name = Format.pp_print_string fmt name

  let indice = Arg.indice

  let indice_opt fmt = function None -> () | Some i -> indice fmt i

  let func_type fmt (l, r) =
    Format.fprintf fmt "(func %a %a)" param_type l result_type r

  let block_type = Arg.bt

  let block_type_opt fmt = function None -> () | Some bt -> block_type fmt bt

  let limits fmt { min; max } =
    match max with
    | None -> Format.fprintf fmt "%d" min
    | Some max -> Format.fprintf fmt "%d %d" min max

  let mem_type fmt t = limits fmt t

  let table_type fmt (mt, rt) =
    Format.fprintf fmt "%a %a" mem_type mt ref_type rt

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
    | Ref_null t -> Format.fprintf fmt "ref.null %a" ref_type t
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
    | I64_load32 (s, ma) -> Format.fprintf fmt "i64.load32_%a %a" sx s memarg ma
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
      Format.fprintf fmt "return_call_indirect %a %a" indice tbl_id block_type ty_id
    | Call id -> Format.fprintf fmt "call %a" indice id
    | Call_indirect (tbl_id, ty_id) ->
      Format.fprintf fmt "call_indirect %a %a" indice tbl_id block_type ty_id

  and expr fmt instrs =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      instr fmt instrs

  let func fmt (f : (Arg.indice, Arg.bt) func') =
    (* TODO: typeuse ? *)
    Format.fprintf fmt "(func %a %a %a@\n  @[<v>%a@]@\n)" id_opt f.id block_type
      f.type_f locals f.locals expr f.body

  let funcs fmt (funcs : (Arg.indice, Arg.bt) func' list) =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      func fmt funcs
end

module Input_Expr = Make_Expr (Symbolic_indice)

module Global = struct
  open Input_Expr

  let start fmt start = Format.fprintf fmt "(start %a)" indice start

  let symb_indice_opt fmt = function
    | None -> ()
    | Some i -> Symbolic_indice.indice fmt i

  let export_desc fmt = function
    | Export_func id -> Format.fprintf fmt "(func %a)" symb_indice_opt id
    | Export_table id -> Format.fprintf fmt "(table %a)" symb_indice_opt id
    | Export_mem id -> Format.fprintf fmt "(memory %a)" symb_indice_opt id
    | Export_global id -> Format.fprintf fmt "(global %a)" symb_indice_opt id

  let export fmt e =
    Format.fprintf fmt "(export %a %a)" name e.name export_desc e.desc

  let global fmt (g : global) =
    Format.fprintf fmt "(global %a %a %a)" id_opt g.id global_type g.type_ expr
      g.init

  let mem fmt (id, ty) =
    Format.fprintf fmt "(memory %a %a)" id_opt id mem_type ty

  let table fmt (id, ty) =
    Format.fprintf fmt "(table %a %a)" id_opt id table_type ty

  let import_desc fmt : import_desc -> Unit.t = function
    | Import_func (id, t) ->
      (* TODO: fixme *)
      Format.fprintf fmt "%a %a" id_opt id block_type t
    | Import_table (id, t) ->
      Format.fprintf fmt "(table %a %a)" id_opt id table_type t
    | Import_mem (id, t) ->
      Format.fprintf fmt "(memory %a %a)" id_opt id mem_type t
    | Import_global (id, t) ->
      Format.fprintf fmt "(global %a %a)" id_opt id global_type t

  let import fmt i =
    Format.fprintf fmt {|(import "%a" "%a" %a)|} name i.module_ name i.name
      import_desc i.desc

  let type_ fmt (i, ft) =
    Format.fprintf fmt "(type %a %a)" id_opt i func_type ft

  let data_mode fmt = function
    | Data_passive -> ()
    | Data_active (i, e) ->
      Format.fprintf fmt "(%a %a)" symb_indice_opt i expr e

  let data fmt d = Format.fprintf fmt {|(data %a %S)|} data_mode d.mode d.init

  let elem_mode fmt = function
    | Elem_passive -> ()
    | Elem_declarative -> Format.fprintf fmt "declare"
    | Elem_active (i, e) -> (
      match i with
      | None -> Format.fprintf fmt "(offset %a)" expr e
      | Some i -> Format.fprintf fmt "(table %a) (offset %a)" indice i expr e )

  let elemexpr fmt e = Format.fprintf fmt "(item %a)" expr e

  let elem fmt (e : elem) =
    Format.fprintf fmt "@[<hov 2>(elem %a %a %a %a)@]" id_opt e.id elem_mode
      e.mode ref_type e.type_
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         elemexpr )
      e.init

  let module_field fmt = function
    | MType t -> type_ fmt t
    | MGlobal g -> global fmt g
    | MTable t -> table fmt t
    | MMem m -> mem fmt m
    | MFunc f -> func fmt f
    | MElem e -> elem fmt e
    | MData d -> data fmt d
    | MStart s -> start fmt s
    | MImport i -> import fmt i
    | MExport e -> export fmt e

  let module_ fmt m =
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
    | Const_null rt -> Format.fprintf fmt "ref.null %a" ref_type rt
    | Const_host i -> Format.fprintf fmt "ref.extern %d" i

  let consts fmt c =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
      (fun fmt c -> Format.fprintf fmt "(%a)" const c)
      fmt c

  let result_const fmt = function
    | Literal c -> const fmt c
    | Nan_canon n -> Format.fprintf fmt "float%a.const nan:canonical" nn n
    | Nan_arith n -> Format.fprintf fmt "float%a.const nan:arithmetic" nn n

  let result fmt = function
    | Result_const c -> Format.fprintf fmt "(%a)" result_const c
    | _ -> Log.err "not yet implemented"

  let action fmt = function
    | Invoke (mod_name, name, c) ->
      Format.fprintf fmt "(invoke %a %s %a)" id_opt mod_name name consts c
    | Get _ -> Format.fprintf fmt "<action_get TODO>"

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
      Format.fprintf fmt {|(assert_trap_module %a "%s")|} module_ m f
    | Assert_invalid (m, msg) ->
      Format.fprintf fmt "(assert_invalid@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)"
        module_ m msg
    | Assert_unlinkable (m, msg) ->
      Format.fprintf fmt "(assert_unlinkable@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)"
        module_ m msg
    | Assert_malformed (m, msg) ->
      Format.fprintf fmt "(assert_malformed@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)"
        module_ m msg
    | Assert_malformed_quote (ls, msg) ->
      Format.fprintf fmt
        "(assert_malformed_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls msg
    | Assert_invalid_quote (ls, msg) ->
      Format.fprintf fmt "(assert_invalid_quote@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)"
        strings ls msg
    | Assert_malformed_binary (ls, msg) ->
      Format.fprintf fmt
        "(assert_malformed_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls
        msg
    | Assert_invalid_binary (ls, msg) ->
      Format.fprintf fmt
        "(assert_invalid_binary@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" strings ls msg

  let cmd fmt = function
    | Module m -> module_ fmt m
    | Assert a -> assert_ fmt a
    | Register (s, name) -> register fmt (s, name)
    | Action _a -> Format.fprintf fmt "<action>"

  let file fmt l =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
      cmd fmt l

  let pos out { Ppxlib.pos_lnum; pos_cnum; pos_bol; _ } =
    Format.fprintf out "line %d:%d" pos_lnum (pos_cnum - pos_bol)
end

module Input = struct
  include Input_Expr
  include Global
end

module Simplified = struct
  include Make_Expr (Simplified_indice)
end

let pp_id fmt = function
  | None -> Format.fprintf fmt "Unnamed"
  | Some name -> Format.fprintf fmt "%s" name
