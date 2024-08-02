(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

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
  { typ : binary global_type (* TODO: init : binary+const expr*)
  ; init : binary expr
  ; id : string option
  }

type data_mode =
  | Data_passive
  (* TODO: Data_active binary+const expr*)
  | Data_active of int * binary expr

type data =
  { id : string option
  ; init : string
  ; mode : data_mode
  }

type elem_mode =
  | Elem_passive
  (* TODO: Elem_active binary+const expr*)
  | Elem_active of int option * binary expr
  | Elem_declarative

type elem =
  { id : string option
  ; typ : binary ref_type (* TODO: init : binary+const expr*)
  ; init : binary expr list
  ; mode : elem_mode
  }

type custom =
  | Uninterpreted of string
  | From_annot of binary Annot.annot

type modul =
  { id : string option
  ; types : binary rec_type Named.t
  ; global : (global, binary global_type) Runtime.t Named.t
  ; table : (binary table, binary table_type) Runtime.t Named.t
  ; mem : (mem, limits) Runtime.t Named.t
  ; func : (binary func, binary block_type) Runtime.t Named.t
      (* TODO: switch to func_type *)
  ; elem : elem Named.t
  ; data : data Named.t
  ; exports : exports
  ; start : int option
  ; custom : custom list
  }

let empty_modul =
  { id = None
  ; types = Named.empty
  ; global = Named.empty
  ; table = Named.empty
  ; mem = Named.empty
  ; func = Named.empty
  ; elem = Named.empty
  ; data = Named.empty
  ; exports = { global = []; mem = []; table = []; func = [] }
  ; start = None
  ; custom = []
  }
