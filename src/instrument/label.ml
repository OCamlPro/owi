(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Coverage_criteria = struct
  type t =
    | Function_coverage
    | Statement_coverage
    | Decision_coverage

  let of_string = function
    | "fc" -> Ok Function_coverage
    | "sc" -> Ok Statement_coverage
    | "dc" -> Ok Decision_coverage
    | s -> Fmt.error_msg "unknown coverage criteria %s" s

  let pp fmt criteria =
    Fmt.string fmt
    @@
    match criteria with
    | Function_coverage -> "fc"
    | Statement_coverage -> "sc"
    | Decision_coverage -> "dc"
end

let annotate_fc _m = assert false

let annotate_sc _m = assert false

let annotate_dc _m = assert false

let annotate criteria m =
  match criteria with
  | Coverage_criteria.Function_coverage -> annotate_fc m
  | Statement_coverage -> annotate_sc m
  | Decision_coverage -> annotate_dc m
