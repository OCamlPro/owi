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

  val to_work_ds_module : t -> seed:int option -> (module Prio.S)
end

type t =
  { deterministic_result_order : bool
  ; entry_point : string option
  ; exploration_strategy : Exploration_strategy.t
  ; fail_mode : fail_mode
  ; invoke_with_symbols : bool
  ; model_format : Model.output_format
  ; model_out_file : Fpath.t option
  ; no_assert_failure_expression_printing : bool
  ; no_stop_at_failure : bool
  ; no_value : bool
  ; no_worker_isolation : Bool.t
  ; seed : int option
  ; solver : Smtml.Solver_type.t
  ; timeout : float option
  ; timeout_instr : int option
  ; unsafe : bool
  ; use_ite_for_select : bool
  ; with_breadcrumbs : bool
  ; workers : Int.t Option.t
  ; workspace : Fpath.t option
  }
