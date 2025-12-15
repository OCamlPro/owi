type fail_mode =
  | Trap_only
  | Assertion_only
  | Both

module Exploration_strategy : sig
  type t =
    | FIFO
    | LIFO
    | Random
    | Random_unseen_then_random
    | Rarity
    | Hot_path_penalty
    | Rarity_aging
    | Rarity_depth_aging
    | Rarity_depth_loop_aging
    | Rarity_depth_loop_aging_random

  val of_string : String.t -> (t, [ `Msg of string ]) Prelude.Result.t

  val pp : t Fmt.t

  val to_work_ds_module : t -> (module Prio.S)
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
