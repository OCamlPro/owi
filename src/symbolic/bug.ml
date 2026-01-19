type t =
  { kind : [ `Trap of Result.err | `Assertion of Symbolic_boolean.t ]
  ; model : Smtml.Model.t
  ; thread : Thread.t
  }

let is_trap { kind; _ } =
  match kind with `Assertion _ -> false | `Trap _ -> true

let is_assertion { kind; _ } =
  match kind with `Assertion _ -> true | `Trap _ -> false

let compare_breadcrumbs bug1 bug2 =
  let breadcrumbs1 = List.rev @@ bug1.thread.breadcrumbs in
  let breadcrumbs2 = List.rev @@ bug2.thread.breadcrumbs in
  List.compare compare breadcrumbs1 breadcrumbs2

let sort_seq_if b seq =
  if b then List.of_seq seq |> List.sort compare_breadcrumbs |> List.to_seq
  else seq
