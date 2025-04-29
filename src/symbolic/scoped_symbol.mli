type scope_token =
  | Open_scope of string
  | Close_scope
  | Symbol of Smtml.Symbol.t

val symbol : Smtml.Symbol.t -> scope_token

val only_symbols : scope_token list -> Smtml.Symbol.t list

val to_scfg :
     no_value:bool
  -> Smtml.Model.t
  -> scope_token list
  -> Scfg.Types.directive list

val to_json :
     Smtml.Model.t
  -> scope_token list
  -> [> `Assoc of (string * [> `List of Yojson.Basic.t list ]) list ]
