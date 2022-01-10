open Types

type t = const list

exception Empty

let push s v = v :: s

let push_bool s b = push s (Const_I32 (if b then 1l else 0l))

let push_i32 s i = push s (Const_I32 i)

let push_i32_of_int s i = push_i32 s (Int32.of_int i)

let push_i64 s i = push s (Const_I64 i)

let push_i64_of_int s i = push_i64 s (Int64.of_int i)

let push_f32 s f = push s (Const_F32 f)

let push_f64 s f = push s (Const_F64 f)

let push_host s n = push s (Const_host n)

let pp fmt s =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
    Pp.const fmt s

let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)

let drop = function [] -> raise Empty | _hd :: tl -> tl

let pop_i32 s =
  let hd, tl = pop s in
  match hd with
  | Const_I32 n -> (n, tl)
  | _ | (exception Empty) -> failwith "invalid type (expected i32)"

let pop_i32_to_int s =
  let hd, tl = pop_i32 s in
  (Int32.to_int hd, tl)

let pop_i32_to_char s =
  let hd, tl = pop_i32 s in
  (Char.chr (Int32.to_int hd mod 256), tl)

let pop2_i32 s =
  try
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | Const_I32 n1, Const_I32 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected i32)"
  with Empty -> failwith "invalid type (expected i32)"

let pop_i64 s =
  try
    let hd, tl = pop s in
    match hd with
    | Const_I64 n -> (n, tl)
    | _ -> failwith "invalid type (expected i64)"
  with Empty -> failwith "invalid type (expected i64)"

let pop2_i64 s =
  try
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | Const_I64 n1, Const_I64 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected i64)"
  with Empty -> failwith "invalid type (expected i64)"

let pop_f32 s =
  try
    let hd, tl = pop s in
    match hd with
    | Const_F32 f -> (f, tl)
    | _ -> failwith "invalid type (expected f32)"
  with Empty -> failwith "invalid type (expected f32)"

let pop2_f32 s =
  try
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | Const_F32 n1, Const_F32 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected f32)"
  with Empty -> failwith "invalid type (expected f32)"

let pop_f64 s =
  try
    let hd, tl = pop s in
    match hd with
    | Const_F64 f -> (f, tl)
    | _ -> failwith "invalid type (expected f64)"
  with Empty -> failwith "invalid type (expected f64)"

let pop2_f64 s =
  try
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | Const_F64 n1, Const_F64 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected f64)"
  with Empty -> failwith "invalid type (expected f64)"

let pop_host s =
  try
    let hd, tl = pop s in
    match hd with
    | Const_host n -> (n, tl)
    | _ -> failwith "invalid type (expected host)"
  with Empty -> failwith "invalid type (expected host)"

let pop_ref s =
  try
    let hd, tl = pop s in
    match hd with
    | Const_host _ | Const_null _ -> (hd, tl)
    | _ -> failwith "invalid type (expected ref)"
  with Empty -> failwith "invalid type (expected ref)"

let pop_bool s =
  try
    let hd, tl = pop s in
    match hd with
    | Const_I32 n -> (n <> 0l, tl)
    | _ -> failwith "invalid type (expected i32 (bool))"
  with Empty -> failwith "invalid type (expected i32 (bool))"

let pop_is_null s =
  try
    let hd, tl = pop s in
    match hd with
    | Const_null _t -> (true, tl)
    | Const_host _t -> (false, tl)
    | _ -> failwith "invalid type (expected const_null)"
  with Empty -> failwith "invalid type (expected const_null)"

let pop_n s n =
  (List.filteri (fun i _hd -> i < n) s, List.filteri (fun i _hd -> i >= n) s)

let keep s n = List.filteri (fun i _hd -> i < n) s
