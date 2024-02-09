module Concrete : sig
  val modul :
       Concrete.P.env Env_id.collection
    -> Concrete.P.Module_to_run.t
    -> unit Result.t

  val exec_vfunc_from_outside :
       locals:V.t list
    -> env:Link_env.t'
    -> envs:Concrete.P.env Env_id.collection
    -> Func_intf.t
    -> V.t list Result.t

  val exec_ibinop : V.t list -> Types.nn -> Types.ibinop -> V.t list

  val exec_iunop : V.t list -> Types.nn -> Types.iunop -> V.t list

  val exec_itestop : V.t list -> Types.nn -> Types.itestop -> V.t list

  val exec_irelop : V.t list -> Types.nn -> Types.irelop -> V.t list

  val exec_itruncf : V.t list -> Types.nn -> Types.nn -> Types.sx -> V.t list

  val exec_itruncsatf : V.t list -> Types.nn -> Types.nn -> Types.sx -> V.t list

  val exec_ireinterpretf : V.t list -> Types.nn -> Types.nn -> V.t list

  val exec_fbinop : V.t list -> Types.nn -> Types.fbinop -> V.t list

  val exec_funop : V.t list -> Types.nn -> Types.funop -> V.t list

  val exec_frelop : V.t list -> Types.nn -> Types.frelop -> V.t list

  val exec_fconverti : V.t list -> Types.nn -> Types.nn -> Types.sx -> V.t list

  val exec_freinterpreti : V.t list -> Types.nn -> Types.nn -> V.t list
end

module SymbolicP : sig
  val modul :
       Symbolic.P.env Env_id.collection
    -> Symbolic.P.Module_to_run.t
    -> unit Result.t Symbolic.P.Choice.t
end

module SymbolicM : sig
  val modul :
       Symbolic.M.env Env_id.collection
    -> Symbolic.M.Module_to_run.t
    -> unit Result.t Symbolic.M.Choice.t
end
