include Symbolic_choice.Make (Thread_with_memory)

let lift_mem (mem_op : 'a Symbolic_choice_without_memory.t) : 'a t =
  Symbolic_choice.CoreImpl.State.project_state Thread_with_memory.project
    Thread_with_memory.restore mem_op
