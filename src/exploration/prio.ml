type metrics =
  { instr_counter : int Option.t
  ; distance_to_unreachable : int Option.t
  ; depth : int
  }

let v ~instr_counter ~distance_to_unreachable ~depth =
  { instr_counter; distance_to_unreachable; depth }

let dummy = { instr_counter = None; distance_to_unreachable = None; depth = 0 }

let low =
  { instr_counter = Some 100_000; distance_to_unreachable = None; depth = 0 }

(* In the following, we use the convention that a path with a *high priority* (i.e. that should be explored first) corresponds to a path with a high "integer". *)

let compare_with_highest_first l r = Int.compare r l [@@inline]

let unknown_instruction_counter = 100

let max_rarity = 1_000_000

let use_logarithmic_depth = true

let depth_scale = 4.

let max_depth = if use_logarithmic_depth then 20 else 1_000

let max_loop_penalty = 10_000

let unknown_loop_penalty = max_loop_penalty / 10

let rarity_of_metrics { instr_counter; _ } =
  let c =
    match instr_counter with Some c -> c | None -> unknown_instruction_counter
  in
  let c = min c max_rarity in
  max_int - c

let depth_of_metrics { depth; _ } =
  let depth =
    if use_logarithmic_depth then
      let depth = Float.of_int depth in
      let depth = depth_scale *. log (depth +. 1.) in
      Int.of_float depth
    else depth
  in
  min depth max_depth

let loop_penalty_of_metrics { instr_counter; _ } =
  match instr_counter with
  | Some c -> ~-(min (c * c) max_loop_penalty)
  | None -> ~-unknown_loop_penalty

module type T = sig
  type t

  val of_metrics : metrics -> t

  val compare : t -> t -> int

  val requires_random : Bool.t
end

module FIFO : T = struct
  type t = int

  let of_metrics =
    let counter = Atomic.make max_int in
    fun { instr_counter = _; distance_to_unreachable = _; depth = _ } ->
      Atomic.fetch_and_add counter ~-1

  let compare = compare_with_highest_first

  let requires_random = false
end

module LIFO : T = struct
  type t = int

  let of_metrics =
    let counter = Atomic.make 0 in
    fun { instr_counter = _; distance_to_unreachable = _; depth = _ } ->
      Atomic.fetch_and_add counter 1

  let compare = compare_with_highest_first

  let requires_random = false
end

module Random_prio : T = struct
  type t = int

  let of_metrics { instr_counter = _; distance_to_unreachable = _; depth = _ } =
    Random.int 10_000

  let compare = compare_with_highest_first

  let requires_random = true
end

module Random_unseen_then_random : T = struct
  type t = int

  let of_metrics { instr_counter; distance_to_unreachable = _; depth = _ } =
    match instr_counter with
    | Some 0 -> max_int - Random.int 10_000
    | None | Some _ -> Random.int 10_000

  let compare = compare_with_highest_first

  let requires_random = true
end

module Rarity : T = struct
  type t = int

  let of_metrics s = rarity_of_metrics s

  let compare = compare_with_highest_first

  let requires_random = false
end

module Hot_path_penalty : T = struct
  type t = int

  let of_metrics s = loop_penalty_of_metrics s

  let compare = compare_with_highest_first

  let requires_random = false
end

module Rarity_aging : T = struct
  type t =
    { rarity : int
    ; age : int
    }

  let age_counter = Atomic.make 0

  let of_metrics s =
    let rarity = rarity_of_metrics s in
    let age = Atomic.fetch_and_add age_counter ~-1 in
    { rarity; age }

  let compare p1 p2 =
    let rarity = compare_with_highest_first p1.rarity p2.rarity in
    if rarity = 0 then compare_with_highest_first p1.age p2.age else rarity

  let requires_random = false
end

module Rarity_depth_aging : T = struct
  type t =
    { rarity : int
    ; depth : int
    ; age : int
    }

  let age_counter = Atomic.make 0

  let of_metrics s =
    let rarity = rarity_of_metrics s in
    let depth = depth_of_metrics s in
    let age = Atomic.fetch_and_add age_counter ~-1 in

    { rarity; depth; age }

  let compare p1 p2 =
    let rarity = compare_with_highest_first p1.rarity p2.rarity in
    if rarity = 0 then
      let depth = compare_with_highest_first p1.depth p2.depth in
      if depth = 0 then compare_with_highest_first p1.age p2.age else depth
    else rarity

  let requires_random = false
end

module Rarity_depth_loop_aging : T = struct
  type t =
    { rarity : int
    ; depth : int
    ; loop_penalty : int
    ; age : int
    }

  let age_counter = Atomic.make 0

  let of_metrics s =
    let rarity = rarity_of_metrics s in
    let depth = depth_of_metrics s in
    let loop_penalty = loop_penalty_of_metrics s in
    let age = Atomic.fetch_and_add age_counter ~-1 in

    { rarity; depth; loop_penalty; age }

  let compare p1 p2 =
    let rarity = compare_with_highest_first p1.rarity p2.rarity in
    if rarity = 0 then
      let depth = compare_with_highest_first p1.depth p2.depth in
      if depth = 0 then
        let loop_penalty =
          compare_with_highest_first p1.loop_penalty p2.loop_penalty
        in
        if loop_penalty = 0 then compare_with_highest_first p1.age p2.age
        else loop_penalty
      else depth
    else rarity

  let requires_random = false
end

module Rarity_depth_loop_aging_random : T = struct
  type t =
    { rarity : int
    ; depth : int
    ; loop_penalty : int
    ; age : int
    ; random : int
    }

  let age_counter = Atomic.make 0

  let of_metrics s =
    let rarity = rarity_of_metrics s in
    let depth = depth_of_metrics s in
    let loop_penalty = loop_penalty_of_metrics s in
    let age = Atomic.fetch_and_add age_counter ~-1 in
    let random = Random.int 7 in

    { rarity; depth; loop_penalty; age; random }

  let compare p1 p2 =
    let rarity = compare_with_highest_first p1.rarity p2.rarity in
    if rarity = 0 then
      let depth = compare_with_highest_first p1.depth p2.depth in
      if depth = 0 then
        let loop_penalty =
          compare_with_highest_first p1.loop_penalty p2.loop_penalty
        in
        if loop_penalty = 0 then
          let age = compare_with_highest_first p1.age p2.age in
          if age = 0 then compare_with_highest_first p1.random p2.random
          else age
        else loop_penalty
      else depth
    else rarity

  let requires_random = true
end

module type S = sig
  type !'a t

  (** Create a new queue *)
  val make : unit -> 'a t

  (** Add a new element to the queue *)
  val push : 'a -> metrics -> 'a t -> unit

  (** Make a new pledge, ie indicate that new elements may be pushed to the
      queue and that calls to pop should block waiting for them. *)
  val new_pledge : 'a t -> unit

  (** End one pledge. *)
  val end_pledge : 'a t -> unit

  (** Mark the queue as closed: all threads trying to pop from it will get no
      element. *)
  val close : 'a t -> unit

  (** Pop all elements from the queue in a lazy Seq.t, *)
  val read_as_seq : 'a t -> 'a Seq.t

  val work_while : ('a -> (metrics * 'a -> unit) -> unit) -> 'a t -> unit
end

module Make (P : T) : S = struct
  module Priority_queue = Priority_queue.Make (P)

  type 'a t = ('a, metrics * 'a) Synchronizer.t

  let pop q ~pledge = Synchronizer.get q ~pledge

  let new_pledge = Synchronizer.new_pledge

  let end_pledge = Synchronizer.end_pledge

  let rec read_as_seq (q : 'a t) : 'a Seq.t =
   fun () ->
    match pop q ~pledge:false with
    | None -> Nil
    | Some v -> Cons (v, read_as_seq q)

  let push v prio q = Synchronizer.write q (prio, v)

  let work_while f q = Synchronizer.work_while f q

  let close = Synchronizer.close

  let writter (prio, v) q =
    let prio = P.of_metrics prio in
    let prio_and_value = (prio, v) in
    Priority_queue.push prio_and_value q

  let make () =
    let q = Priority_queue.empty () in
    let writter prio_and_value = writter prio_and_value q in
    Synchronizer.init (fun () -> Priority_queue.pop q) writter
end
