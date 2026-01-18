type t =
  { kind : [ `Trap of Result.err | `Assertion of Symbolic_boolean.t ]
  ; model : Smtml.Model.t
      (* TODO: all that follows could be replaced by the Thread directly! *)
  ; labels : (int * string) list
  ; breadcrumbs : int list
  ; symbol_scopes : Symbol_scope.t
  }

let is_trap { kind; _ } =
  match kind with `Assertion _ -> false | `Trap _ -> true

let is_assertion { kind; _ } =
  match kind with `Assertion _ -> true | `Trap _ -> false
