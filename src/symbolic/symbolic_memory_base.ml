open Smtml

(* TODO: These functions should be in smtml *)

(* TODO: don't rebuild so many values it generates unecessary hc lookups *)
let merge_extracts (e1, h, m1) (e2, m2, l) =
  let ty = Expr.ty e1 in
  if m1 = m2 && Expr.equal e1 e2 then
    if h - l = Ty.size ty then e1 else Expr.make (Extract (e1, h, l))
  else
    Expr.(make (Concat (make (Extract (e1, h, m1)), make (Extract (e2, m2, l)))))

let concat ~msb ~lsb offset =
  assert (offset > 0 && offset <= 8);
  match (Expr.view msb, Expr.view lsb) with
  | Val (Num (I8 i1)), Val (Num (I8 i2)) ->
    Symbolic_value.const_i32 Int32.(logor (shl (of_int i1) 8l) (of_int i2))
  | Val (Num (I8 i1)), Val (Num (I32 i2)) ->
    let offset = offset * 8 in
    if offset < 32 then
      Symbolic_value.const_i32 Int32.(logor (shl (of_int i1) (of_int offset)) i2)
    else
      let i1' = Int64.of_int i1 in
      let i2' = Int64.of_int32 i2 in
      Symbolic_value.const_i64 Int64.(logor (shl i1' (of_int offset)) i2')
  | Val (Num (I8 i1)), Val (Num (I64 i2)) ->
    let offset = Int64.of_int (offset * 8) in
    Symbolic_value.const_i64 Int64.(logor (shl (of_int i1) offset) i2)
  | Extract (e1, h, m1), Extract (e2, m2, l) ->
    merge_extracts (e1, h, m1) (e2, m2, l)
  | Extract (e1, h, m1), Concat ({ node = Extract (e2, m2, l); _ }, e3) ->
    Expr.(make (Concat (merge_extracts (e1, h, m1) (e2, m2, l), e3)))
  | _ -> Expr.make (Concat (msb, lsb))

let extract v pos =
  match Expr.view v with
  | Val (Num (I8 _)) -> v
  | Val (Num (I32 i)) ->
    let i' = Int32.(to_int @@ logand 0xffl @@ shr_s i @@ of_int (pos * 8)) in
    Expr.value (Num (I8 i'))
  | Val (Num (I64 i)) ->
    let i' = Int64.(to_int @@ logand 0xffL @@ shr_s i @@ of_int (pos * 8)) in
    Expr.value (Num (I8 i'))
  | Cvtop
      (_, Zero_extend 24, ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym))
  | Cvtop
      (_, Sign_extend 24, ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym))
    ->
    sym
  | _ -> Expr.make (Extract (v, pos + 1, pos))
