module Stack = struct
  type 'el t = 'el list

  let empty : 'el t = []

  let pop : 'el t -> 'el * 'el t = function
    | [] -> Fmt.failwith "pop on empty stack"
    | h :: t -> (h, t)

  let push : 'el t -> 'el -> 'el t = fun stack v -> v :: stack

  let pop_n : 'el t -> int -> ('el list * 'el t) Result.t =
   fun stack n ->
    if List.length stack > n then Ok (List.take n stack, List.drop n stack)
    else Fmt.error_msg ""

  let pop_2 : 'el t -> ('el * 'el * 'el t) Result.t = function
    | v1 :: v2 :: stack' -> Ok (v1, v2, stack')
    | _ -> Fmt.error_msg "no enough values to pop"

  let pp : 'el Fmt.t -> Format.formatter -> 'el t -> unit =
   fun el_fmt fmt t -> Fmt.pf fmt "[%a]" (Fmt.list ~sep:Fmt.comma el_fmt) t

  let is_empty : 'el t -> bool = fun t -> List.is_empty t

  let to_list : 'el t -> 'el list = fun t -> t

  let of_list : 'el list -> 'el t = fun t -> t

  let append : 'el t -> 'el t -> 'el t = fun x y -> List.append x y

  let take : int -> 'el t -> 'el t = List.take

  let rev : 'el t -> 'el t = List.rev
end
