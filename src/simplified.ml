(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Format
open Types

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
  { typ : simplified global_type
  ; init : simplified Const.expr
  ; id : string option
  }

type data_mode =
  | Data_passive
  | Data_active of int option * simplified Const.expr

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

type elem_mode =
  | Elem_passive
  | Elem_active of int option * simplified Const.expr
  | Elem_declarative

type elem =
  { id : string option
  ; typ : simplified ref_type
  ; init : simplified Const.expr list
  ; mode : elem_mode
  }

type modul =
  { id : string option
  ; global : (global, simplified global_type) Runtime.t Named.t
  ; table : (simplified table, simplified table_type) Runtime.t Named.t
  ; mem : (mem, limits) Runtime.t Named.t
  ; func :
      (simplified func, (simplified, simplified) block_type) Runtime.t Named.t
      (* TODO: switch to func_type *)
  ; elem : elem Named.t
  ; data : data Named.t
  ; exports : exports
  ; start : int option
  }

module Pp = struct
  let id fmt = Option.iter (fun id -> pp fmt " $%s" id)

  let func fmt = function
    | Runtime.Local f -> Types.Pp.func fmt f
    | Runtime.Imported { Imported.modul; name; _ } -> pp fmt "%s.%s" modul name

  let lst f fmt l =
    (pp_list ~pp_sep:(fun fmt () -> pp fmt "@\n") f) fmt (List.rev l)

  let funcs fmt funcs = lst (Indexed.pp func) fmt funcs.Named.values

  let export fmt (export : export) = pp fmt "%s: %d" export.name export.id

  let start fmt = function None -> () | Some ind -> pp fmt "(start %d)" ind

  let modul fmt (m : modul) : unit =
    pp fmt "(module%a@\n  @[%a@\n%a@]@\n)" id m.id funcs m.func start m.start
end
