open Stack_intf

module Make (V : Value) :
  Stack_intf.S
    with type value := V.t
     and type vbool := V.vbool
     and type int32 := V.int32
     and type int64 := V.int64
     and type float32 := V.float32
     and type float64 := V.float64
     and type ref_value := V.ref_value = struct
  open V

  type t = V.t list

  exception Empty

  let empty = []

  let push s v = v :: s

  let push_const_bool s b = push s (I32 (V.const_i32 (if b then 1l else 0l)))

  let push_bool s b = push s (I32 (V.Bool.int32 b))

  let push_const_i32 s i = push s (I32 (V.const_i32 i))

  let push_i32 s i = push s (I32 i)

  let push_i32_of_int s i = push_const_i32 s (Int32.of_int i)

  let push_const_i64 s i = push s (I64 (V.const_i64 i))

  let push_i64 s i = push s (I64 i)

  let push_i64_of_int s i = push_const_i64 s (Int64.of_int i)

  let push_const_f32 s f = push s (F32 (V.const_f32 f))

  let push_f32 s f = push s (F32 f)

  let push_const_f64 s f = push s (F64 (V.const_f64 f))

  let push_f64 s f = push s (F64 f)

  let push_as_externref s ty v =
    ignore (s, ty, v);
    failwith "TODO"
  (* push s (V.const_ref (Externref (Some (E (ty, v))))) *)

  let push_array s a =
    ignore (s, a);
    failwith "TODO"
  (* push s (Ref (Arrayref (Some a))) *)

  let pp fmt (s : t) =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
      V.pp fmt s

  let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)

  let drop = function [] -> raise Empty | _hd :: tl -> tl

  let pop_i32 s =
    let hd, tl = pop s in
    match hd with
    | I32 n -> (n, tl)
    | _ | (exception Empty) -> Log.err "invalid type (expected i32)"

  (* let pop_i32_to_int s = *)
  (*   let hd, tl = pop_i32 s in *)
  (*   (Int32.to_int hd, tl) *)

  (* let pop_ui32_to_int s = *)
  (*   let hd, tl = pop_i32 s in *)
  (*   match Int32.unsigned_to_int hd with *)
  (*   | None -> Log.err "invalid type (expected unsigned i32)" *)
  (*   | Some hd -> (hd, tl) *)

  (* let pop_i32_to_char s = *)
  (*   let hd, tl = pop_i32 s in *)
  (*   match Int32.unsigned_to_int hd with *)
  (*   | None -> Log.err "invalid type (expected unsigned i32)" *)
  (*   | Some n -> (Char.chr (n mod 256), tl) *)

  let pop2_i32 s =
    try
      let n2, s = pop s in
      let n1, tl = pop s in
      match (n1, n2) with
      | I32 n1, I32 n2 -> ((n1, n2), tl)
      | _ -> Log.err "invalid type (expected i32)"
    with Empty -> Log.err "invalid type (expected i32)"

  let pop_i64 s =
    try
      let hd, tl = pop s in
      match hd with
      | I64 n -> (n, tl)
      | _ -> Log.err "invalid type (expected i64)"
    with Empty -> Log.err "invalid type (expected i64)"

  let pop2_i64 s =
    try
      let n2, s = pop s in
      let n1, tl = pop s in
      match (n1, n2) with
      | I64 n1, I64 n2 -> ((n1, n2), tl)
      | _ -> Log.err "invalid type (expected i64)"
    with Empty -> Log.err "invalid type (expected i64)"

  let pop_f32 s =
    try
      let hd, tl = pop s in
      match hd with
      | F32 f -> (f, tl)
      | _ -> Log.err "invalid type (expected f32)"
    with Empty -> Log.err "invalid type (expected f32)"

  let pop2_f32 s =
    try
      let n2, s = pop s in
      let n1, tl = pop s in
      match (n1, n2) with
      | F32 n1, F32 n2 -> ((n1, n2), tl)
      | _ -> Log.err "invalid type (expected f32)"
    with Empty -> Log.err "invalid type (expected f32)"

  let pop_f64 s =
    try
      let hd, tl = pop s in
      match hd with
      | F64 f -> (f, tl)
      | _ -> Log.err "invalid type (expected f64)"
    with Empty -> Log.err "invalid type (expected f64)"

  let pop2_f64 s =
    try
      let n2, s = pop s in
      let n1, tl = pop s in
      match (n1, n2) with
      | F64 n1, F64 n2 -> ((n1, n2), tl)
      | _ -> Log.err "invalid type (expected f64)"
    with Empty -> Log.err "invalid type (expected f64)"

  let pop_ref s =
    try
      let hd, tl = pop s in
      match hd with
      | Ref _ -> (hd, tl)
      | _ -> Log.err "invalid type (expected ref)"
    with Empty -> Log.err "invalid type (expected ref)"

  let pop_as_ref s =
    try
      let hd, tl = pop s in
      match hd with
      | Ref hd -> (hd, tl)
      | _ -> Log.err "invalid type (expected ref)"
    with Empty -> Log.err "invalid type (expected ref)"

  (* let pop_as_externref (type ty) (ty : ty Type_id.ty) s : ty * 'env t = *)
  (*   try *)
  (*     let hd, tl = pop s in *)
  (*     match hd with *)
  (*     | Ref (Externref (Some (E (ety, hd)))) -> begin *)
  (*       match Type_id.eq ty ety with *)
  (*       | None -> Log.err "invalid type (externref)" *)
  (*       | Some Eq -> (hd, tl) *)
  (*     end *)
  (*     | _ -> Log.err "invalid type (expected extern ref)" *)
  (*   with Empty -> Log.err "invalid type (expected extern ref)" *)

  let pop_bool s =
    try
      let hd, tl = pop s in
      match hd with
      | I32 n -> (V.I32.ne n (V.const_i32 0l)), tl
      | _ -> Log.err "invalid type (expected i32 (bool))"
    with Empty -> Log.err "invalid type (expected i32 (bool))"

  (* let pop_is_null s = *)
  (*   try *)
  (*     let hd, tl = pop s in *)
  (*     match hd with *)
  (*     | Ref (Externref None | Funcref None) -> (true, tl) *)
  (*     | Ref (Externref (Some _) | Funcref (Some _)) -> (false, tl) *)
  (*     | _ -> Log.err "invalid type (expected const_null)" *)
  (*   with Empty -> Log.err "invalid type (expected const_null)" *)

  let pop_n s n =
    (List.filteri (fun i _hd -> i < n) s, List.filteri (fun i _hd -> i >= n) s)

  let keep s n = List.filteri (fun i _hd -> i < n) s

  let rec drop_n s n =
    if n = 0 then s
    else match s with [] -> invalid_arg "drop_n" | _ :: tl -> drop_n tl (n - 1)
end
