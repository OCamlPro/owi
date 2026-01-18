type 'a t =
  | EVal of 'a
  | EError of
      { kind : [ `Trap of Result.err | `Assertion of Symbolic_boolean.t ]
      ; model : Smtml.Model.t
          (* TODO: all that follows could be replaced by the Thread directly! *)
      ; labels : (int * string) list
      ; breadcrumbs : int list
      ; symbol_scopes : Symbol_scope.t
      }
