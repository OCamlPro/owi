type context := Abstract_domain_intv.Context.t

module Locals : sig
  include PatriciaTree.MAP with type key = Int.t
end

module Stack :
  Abs_stack.S
    with type value := Abstract_value0.t
     and type i32 := Abstract_value0.i32
     and type i64 := Abstract_value0.i64

type t =
  { ctx : context
  ; stack : Stack.t
  ; locals : Abstract_value0.t Locals.t
  ; func_rt : Binary.result_type
  ; env : Abs_extern_func.extern_func Link_env.t (* todo *)
  ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
  }

val join : t -> t -> t

val widen : Codex.Domains.Sig.Widening_Id.t -> t -> t -> t * bool

val assume : t -> Abstract_value0.boolean -> t option

val pp : t Fmt.t

val pp_ctx : t Fmt.t
