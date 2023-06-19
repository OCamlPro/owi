open Types

include struct
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
