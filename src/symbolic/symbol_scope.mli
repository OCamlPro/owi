type token

type t

val symbol : Smtml.Symbol.t -> token

val open_scope : string -> token

val close_scope : token

val empty : t

val push : token -> t -> t

val only_symbols : t -> Smtml.Symbol.t list

val of_expressions : Smtml.Expr.Set.t -> t

val of_symbol : Smtml.Symbol.t -> t

val to_scfg : no_value:bool -> Smtml.Model.t -> t -> Scfg.Types.directive list

val to_json :
     no_value:bool
  -> Smtml.Model.t
  -> t
  -> [> `Assoc of (string * [> `List of Yojson.Basic.t list ]) list ]
