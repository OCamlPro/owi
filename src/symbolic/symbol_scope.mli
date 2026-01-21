type t

val symbol : Smtml.Symbol.t -> t -> t

val open_scope : string -> t -> t

val close_scope : t -> t

val empty : t

val only_symbols : t -> Smtml.Symbol.t list

val of_expressions : Smtml.Expr.Set.t -> t

val of_symbol : Smtml.Symbol.t -> t

val pp : t Fmt.t

val to_scfg : no_value:bool -> Smtml.Model.t -> t -> Scfg.Types.config

val to_json :
     no_value:bool
  -> Smtml.Model.t
  -> t
  -> [> `Assoc of (string * Yojson.Basic.t) list ]

val model_of_json : string -> Smtml.Model.t Result.t

val model_of_scfg : string -> Smtml.Model.t Result.t
