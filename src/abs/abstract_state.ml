module Dom = Abstract_domain_intv

type context = Dom.Context.t

module Locals = PatriciaTree.MakeMap (struct
  include Int

  let to_int i = i
end)

module Stack = Abs_stack.Make (Abstract_value0)

type t =
  { ctx : context
  ; stack : Stack.t
  ; locals : Abstract_value0.t Locals.t
  ; func_rt : Binary.result_type
  ; env : Abs_extern_func.extern_func Link_env.t (* todo *)
  ; envs : Abs_extern_func.extern_func Link_env.t Dynarray.t
  }

let join (_ : t) (_ : t) : t = assert false

let widen : Codex.Domains.Sig.Widening_Id.t -> t -> t -> t * bool =
 fun _ _ _ -> assert false

let assume : t -> Abstract_value0.boolean -> t option = fun _ _ -> assert false

let pp : t Fmt.t = fun _ _ -> assert false

let pp_ctx : t Fmt.t = fun _ _ -> assert false
