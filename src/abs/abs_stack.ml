module Make (M : sig
  type t
end) =
struct
  type t = M.t list

  let empty : M.t list = []

  let pop : t -> M.t * t = function h :: t -> (h, t) | [] -> assert false

  let push : t -> M.t -> t = fun stack v -> v :: stack

  let pop_n : t -> int -> M.t list * t =
   fun stack n -> (List.take n stack, List.drop n stack)

  let pop_2 : t -> M.t * M.t * t = function
    | v1 :: v2 :: stack' -> (v1, v2, stack')
    | _ -> assert false

  let pp : M.t Fmt.t -> Format.formatter -> t -> unit =
   fun el_fmt fmt t -> Fmt.pf fmt "[%a]" (Fmt.list ~sep:(Fmt.any ", ") el_fmt) t

  let is_empty : t -> bool = fun t -> List.is_empty t

  let to_list : t -> M.t list = fun t -> t

  let of_list : M.t list -> t = fun t -> t

  let append : t -> t -> t = fun x y -> List.append x y

  let take : int -> t -> t = List.take

  let drop : int -> t -> t = List.drop

  let rev : t -> t = List.rev

  let length : t -> int = List.length
end
