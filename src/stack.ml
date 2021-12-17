open Types
include Stdlib.Stack

let push s v = push v s

let push_bool s b =
  push s
    (Const_I32
       ( if b then
         1l
       else
         0l ) )

let push_i32 s i = push s (Const_I32 i)

let push_i32_of_int s i = push_i32 s (Int32.of_int i)

let push_i64 s i = push s (Const_I64 i)

let push_i64_of_int s i = push_i64 s (Int64.of_int i)

let push_f32 s f = push s (Const_F32 f)

let push_f64 s f = push s (Const_F64 f)

let push_host s n = push s (Const_host n)

let to_list s = to_seq s |> List.of_seq

let pp fmt s =
  let s = to_list s in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
    Pp.const fmt s

let drop s = ignore (pop s)

let pop_i32 s =
  match pop s with
  | Const_I32 n -> n
  | _
  | (exception Empty) ->
    failwith "invalid type (expected i32)"

let pop_i32_to_int s = Int32.to_int @@ pop_i32 s

let pop2_i32 s =
  try
    let n2 = pop s in
    let n1 = pop s in
    match (n1, n2) with
    | Const_I32 n1, Const_I32 n2 -> (n1, n2)
    | _ -> failwith "invalid type (expected i32)"
  with
  | Empty -> failwith "invalid type (expected i32)"

let pop_i64 s =
  match pop s with
  | Const_I64 n -> n
  | _
  | (exception Empty) ->
    failwith "invalid type (expected i64)"

let pop2_i64 s =
  try
    let n2 = pop s in
    let n1 = pop s in
    match (n1, n2) with
    | Const_I64 n1, Const_I64 n2 -> (n1, n2)
    | _ -> failwith "invalid type (expected i64)"
  with
  | Empty -> failwith "invalid type (expected i64)"

let pop_f32 s =
  match pop s with
  | Const_F32 f -> f
  | _
  | (exception Empty) ->
    failwith "invalid type (expected f32)"

let pop2_f32 s =
  try
    let n2 = pop s in
    let n1 = pop s in
    match (n1, n2) with
    | Const_F32 n1, Const_F32 n2 -> (n1, n2)
    | _ -> failwith "invalid type (expected f32)"
  with
  | Empty -> failwith "invalid type (expected f32)"

let pop_f64 s =
  match pop s with
  | Const_F64 f -> f
  | _
  | (exception Empty) ->
    failwith "invalid type (expected f64)"

let pop2_f64 s =
  try
    let n2 = pop s in
    let n1 = pop s in
    match (n1, n2) with
    | Const_F64 n1, Const_F64 n2 -> (n1, n2)
    | _ -> failwith "invalid type (expected f64)"
  with
  | Empty -> failwith "invalid type (expected f64)"

let pop_bool s =
  match pop s with
  | Const_I32 n -> n <> 0l
  | _
  | (exception Empty) ->
    failwith "invalid type (expected i32 (bool))"

let pop_is_null s =
  match pop s with
  | Const_null _t -> true
  | Const_host _t -> false (* TODO: check that a host is never null ? *)
  | _
  | (exception Empty) ->
    failwith "invalid type (expected const_null)"

let pop_n =
  let rec pop_n s acc n =
    if n = 0 then
      acc
    else
      try pop_n s (pop s :: acc) (n - 1) with
      | Empty -> failwith "invalid type (pop_n)"
  in
  fun s n -> pop_n s [] n

let pop s =
  try pop s with
  | Empty -> failwith "invalid type (pop)"

let rec keep s n =
  if n = 0 then
    for _ = 0 to length s - 1 do
      drop s
    done
  else
    let x = pop s in
    keep s (n - 1);
    push s x
