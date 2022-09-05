open Value
type 'env t = 'env Value.t list

exception Empty

let empty = []

let push s v = v :: s

let push_bool s b = push s (I32 (if b then 1l else 0l))

let push_i32 s i = push s (I32 i)

let push_i32_of_int s i = push_i32 s (Int32.of_int i)

let push_i64 s i = push s (I64 i)

let push_i64_of_int s i = push_i64 s (Int64.of_int i)

let push_f32 s f = push s (F32 f)

let push_f64 s f = push s (F64 f)

let push_as_externref s ty v = push s (Ref (Externref (Some (E (ty, v)))))

let pp fmt (s:'env t) =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
    Value.pp fmt s

let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)

let drop = function [] -> raise Empty | _hd :: tl -> tl

let pop_i32 s =
  let hd, tl = pop s in
  match hd with
  | I32 n -> (n, tl)
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
    | I32 n1, I32 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected i32)"
  with Empty -> failwith "invalid type (expected i32)"

let pop_i64 s =
  try
    let hd, tl = pop s in
    match hd with
    | I64 n -> (n, tl)
    | _ -> failwith "invalid type (expected i64)"
  with Empty -> failwith "invalid type (expected i64)"

let pop2_i64 s =
  try
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | I64 n1, I64 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected i64)"
  with Empty -> failwith "invalid type (expected i64)"

let pop_f32 s =
  try
    let hd, tl = pop s in
    match hd with
    | F32 f -> (f, tl)
    | _ -> failwith "invalid type (expected f32)"
  with Empty -> failwith "invalid type (expected f32)"

let pop2_f32 s =
  try
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | F32 n1, F32 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected f32)"
  with Empty -> failwith "invalid type (expected f32)"

let pop_f64 s =
  try
    let hd, tl = pop s in
    match hd with
    | F64 f -> (f, tl)
    | _ -> failwith "invalid type (expected f64)"
  with Empty -> failwith "invalid type (expected f64)"

let pop2_f64 s =
  try
    let n2, s = pop s in
    let n1, tl = pop s in
    match (n1, n2) with
    | F64 n1, F64 n2 -> ((n1, n2), tl)
    | _ -> failwith "invalid type (expected f64)"
  with Empty -> failwith "invalid type (expected f64)"

let pop_ref s =
  try
    let hd, tl = pop s in
    match hd with
    | Ref _ -> (hd, tl)
    | _ -> failwith "invalid type (expected ref)"
  with Empty -> failwith "invalid type (expected ref)"

let pop_as_ref s =
  try
    let hd, tl = pop s in
    match hd with
    | Ref hd -> (hd, tl)
    | _ -> failwith "invalid type (expected ref)"
  with Empty -> failwith "invalid type (expected ref)"

let pop_as_externref (type ty) (ty : ty Value.Extern_ref.ty) s : ty * 'env t =
  try
    let hd, tl = pop s in
    match hd with
    | Ref (Externref Some (E (ety, hd))) -> begin
        match Value.Extern_ref.eq ty ety with
        | None ->
          failwith "invalid type (externref)"
        | Some Eq ->
          (hd, tl)
      end
    | _ -> failwith "invalid type (expected extern ref)"
  with Empty -> failwith "invalid type (expected extern ref)"

let pop_bool s =
  try
    let hd, tl = pop s in
    match hd with
    | I32 n -> (n <> 0l, tl)
    | _ -> failwith "invalid type (expected i32 (bool))"
  with Empty -> failwith "invalid type (expected i32 (bool))"

let pop_is_null s =
  try
    let hd, tl = pop s in
    match hd with
    | Ref (Externref None | Funcref None) -> (true, tl)
    | Ref (Externref Some _ | Funcref Some _) -> (false, tl)
    | _ -> failwith "invalid type (expected const_null)"
  with Empty -> failwith "invalid type (expected const_null)"

let pop_n s n =
  (List.filteri (fun i _hd -> i < n) s, List.filteri (fun i _hd -> i >= n) s)

let keep s n = List.filteri (fun i _hd -> i < n) s
