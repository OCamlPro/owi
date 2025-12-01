type fail_mode =
  | Trap_only
  | Assertion_only
  | Both

module Exploration_strategy : sig
  type t =
    | FIFO
    | LIFO
    | Random
    | Smart

  val to_work_ds_module : t -> (module Work_ds_intf.S)
end

type t =
  { unsafe : bool
  ; workers : int
  ; no_stop_at_failure : bool
  ; no_value : bool
  ; no_assert_failure_expression_printing : bool
  ; deterministic_result_order : bool
  ; fail_mode : fail_mode
  ; exploration_strategy : Exploration_strategy.t
  ; workspace : Fpath.t option
  ; solver : Smtml.Solver_type.t
  ; model_format : Model.output_format
  ; entry_point : string option
  ; invoke_with_symbols : bool
  ; model_out_file : Fpath.t option
  ; with_breadcrumbs : bool
  ; use_ite_for_select : bool
  }
