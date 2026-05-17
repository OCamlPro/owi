module type S = sig
  type i32

  type i64

  type value

  type t = value list

  val empty : value list

  val pop : t -> value * t

  val push : t -> value -> t

  val pop_n : t -> int -> value list * t

  val pop_2 : t -> value * value * t

  val pop_i32 : t -> i32 * t

  val pop_i64 : t -> i64 * t

  val pop2_i32 : t -> i32 * i32 * t

  val pop2_i64 : t -> i64 * i64 * t

  val pp : value Fmt.t -> Format.formatter -> t -> unit

  val is_empty : t -> bool

  val to_list : t -> value list

  val of_list : value list -> t

  val append : t -> t -> t

  val take : int -> t -> t

  val drop : int -> t -> t

  val rev : t -> t

  val length : t -> int
end

module Make (Value : sig
  type i32

  type i64

  type t =
    | I32 of i32
    | I64 of i64
end) :
  S
    with type i32 := Value.i32
     and type i64 := Value.i64
     and type value := Value.t = struct
  type t = Value.t list

  let empty : Value.t list = []

  let pop : t -> Value.t * t = function h :: t -> (h, t) | [] -> assert false

  let pop_i32 : t -> Value.i32 * t = function
    | I32 i :: tl -> (i, tl)
    | _ -> assert false

  let pop_i64 : t -> Value.i64 * t = function
    | I64 i :: tl -> (i, tl)
    | _ -> assert false

  let pop2_i32 : t -> Value.i32 * Value.i32 * t = function
    | I32 i1 :: I32 i2 :: tl -> (i1, i2, tl)
    | _ -> assert false

  let pop2_i64 : t -> Value.i64 * Value.i64 * t = function
    | I64 i1 :: I64 i2 :: tl -> (i1, i2, tl)
    | _ -> assert false

  let push : t -> Value.t -> t = fun stack v -> v :: stack

  let pop_n : t -> int -> Value.t list * t =
   fun stack n -> (List.take n stack, List.drop n stack)

  let pop_2 : t -> Value.t * Value.t * t = function
    | v1 :: v2 :: stack' -> (v1, v2, stack')
    | _ -> assert false

  let pp : Value.t Fmt.t -> Format.formatter -> t -> unit =
   fun el_fmt fmt t -> Fmt.pf fmt "[%a]" (Fmt.list ~sep:(Fmt.any ", ") el_fmt) t

  let is_empty : t -> bool = fun t -> List.is_empty t

  let to_list : t -> Value.t list = fun t -> t

  let of_list : Value.t list -> t = fun t -> t

  let append : t -> t -> t = fun x y -> List.append x y

  let take : int -> t -> t = List.take

  let drop : int -> t -> t = List.drop

  let rev : t -> t = List.rev

  let length : t -> int = List.length
end
