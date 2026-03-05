module Stack = struct
  type t = Abs_value.t list

  (* let pop : t -> (Abs_value.t * t) Result.t = function *)
  (*   | [] -> Fmt.error_msg "pop on empty stack" *)
  (*   | h :: t -> Ok (h, t) *)

  let pop : t -> Abs_value.t * t = function
    | [] -> Fmt.failwith "pop on empty stack"
    | h :: t -> (h, t)

  let push : t -> Abs_value.t -> t = fun stack v -> v :: stack

  (* let pop_n : t -> int -> (t * t) Result.t = *)
  (*  fun stack n -> *)
  (*   let popped = List.take n stack in *)
  (*   if List.length popped = n then Ok (popped, List.drop n stack) *)
  (*   else Fmt.error_msg "not enough values to pop" *)

  let pop_n : t -> int -> t * t =
   fun stack n ->
    let popped = List.take n stack in
    if List.length popped = n then (popped, List.drop n stack)
    else Fmt.failwith "not enough values to pop"
end
