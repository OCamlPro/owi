open Types

include struct
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
