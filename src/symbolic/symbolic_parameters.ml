type fail_mode =
  | Trap_only
  | Assertion_only
  | Both

module Exploration_strategy = struct
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

  let to_priority_module : t -> (module Prio.T) = function
    | LIFO -> (module Prio.LIFO : Prio.T)
    | FIFO -> (module Prio.FIFO)
    | Random -> (module Prio.Random_prio)
    | Random_unseen_then_random -> (module Prio.Random_unseen_then_random)
    | Rarity -> (module Prio.Rarity)
    | Hot_path_penalty -> (module Prio.Hot_path_penalty)
    | Rarity_aging -> (module Prio.Rarity_aging)
    | Rarity_depth_aging -> (module Prio.Rarity_depth_aging)
    | Rarity_depth_loop_aging -> (module Prio.Rarity_depth_loop_aging)
    | Rarity_depth_loop_aging_random -> (module Prio.Rarity_depth_loop_aging)

  let to_work_ds_module strategy ~seed : (module Prio.S) =
    let module M = (val to_priority_module strategy) in
    if M.requires_random then Init.random_state seed;
    (module Prio.Make (M))

  let pp fmt v =
    Fmt.string fmt
    @@
    match v with
    | FIFO -> "fifo"
    | LIFO -> "lifo"
    | Random -> "random"
    | Random_unseen_then_random -> "random-unseen-then-random"
    | Rarity -> "rarity"
    | Hot_path_penalty -> "hot-path-penalty"
    | Rarity_aging -> "rarity-aging"
    | Rarity_depth_aging -> "rarity-depth-aging"
    | Rarity_depth_loop_aging -> "rarity-depth-loop-aging"
    | Rarity_depth_loop_aging_random -> "rarity-depth-loop-aging"

  let of_string s =
    match String.lowercase_ascii s with
    | "fifo" -> Ok FIFO
    | "lifo" -> Ok LIFO
    | "random" -> Ok Random
    | "random-unseen-then-random" -> Ok Random_unseen_then_random
    | "rarity" -> Ok Rarity
    | "hot-path-penalty" -> Ok Hot_path_penalty
    | "rarity-aging" -> Ok Rarity_aging
    | "rarity-depth-aging" -> Ok Rarity_depth_aging
    | "rarity-depth-loop-aging" -> Ok Rarity_depth_loop_aging
    | "rarity-depth-loop-aging-random" -> Ok Rarity_depth_loop_aging_random
    | s -> Fmt.error_msg "unknown exploration strategy %s" s
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
