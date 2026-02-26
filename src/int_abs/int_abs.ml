open Binary

module Sign = struct
  type t =
    | Bot
    | Zero
    | Pos
    | Neg
    | Top

  let pp (fmt : Format.formatter) = function
    | Bot -> Fmt.string fmt "bot"
    | Zero -> Fmt.string fmt "zero"
    | Pos -> Fmt.string fmt "pos"
    | Neg -> Fmt.string fmt "neg"
    | Top -> Fmt.string fmt "top"
end

module Sign_Lattice = struct
  type t = Sign.t

  let join : t -> t -> t =
   fun a b ->
    match (a, b) with
    | Pos, Pos -> Pos
    | Neg, Neg -> Neg
    | Zero, Zero -> Zero
    | Bot, x | x, Bot -> x
    | _ -> Top

  let inter : t -> t -> t =
   fun a b ->
    match (a, b) with
    | Bot, _ | _, Bot -> Bot
    | Pos, Pos -> Pos
    | Neg, Neg -> Neg
    | Zero, Zero -> Zero
    | Pos, Zero | Zero, Pos | Neg, Zero | Zero, Neg -> Bot
    | Pos, Neg | Neg, Pos -> Bot
    | Top, x | x, Top -> x

  let includes : t -> t -> bool =
   fun a b ->
    match (a, b) with
    | Top, _ -> true
    | _, Bot -> true
    | Pos, Pos -> true
    | Neg, Neg -> true
    | Zero, Zero -> true
    | _ -> false
end

module SVA_Sign = struct
  include Sign_Lattice

  module Integer_Forward = struct
    let add : Sign.t -> Sign.t -> Sign.t =
     fun a b ->
      match (a, b) with
      | Bot, _ | _, Bot -> Bot
      | Top, _ | _, Top -> Top
      | Zero, Zero -> Zero
      | Pos, Zero | Pos, Pos | Zero, Pos -> Pos
      | Neg, Zero | Neg, Neg | Zero, Neg -> Neg
      | Pos, Neg | Neg, Pos -> Top

    let sub : Sign.t -> Sign.t -> Sign.t =
     fun a b ->
      match (a, b) with
      | Bot, _ | _, Bot -> Bot
      | Top, _ | _, Top -> Top
      | Zero, Zero -> Zero
      | Pos, Zero | Zero, Neg -> Pos
      | Neg, Zero | Zero, Pos -> Neg
      | Pos, Pos | Neg, Neg -> Top
      | Pos, Neg | Neg, Pos -> Top

    let eq lhs rhs = lhs = rhs
  end

  module Integer_Backward = struct
    let add : t -> t -> t = fun x res -> Integer_Forward.sub res x

    let sub : t -> t -> t = fun x res -> Integer_Forward.add res x
  end
end

type value = Sign_Lattice.t

type state = value list

let exec_iunop _stack : Text.iunop -> value list = assert false

let rec exec_instr stack : instr -> value list = function
  | I32_const i -> (
    match i with
    | 0l -> Zero :: stack
    | i when Int32.lt i 0l -> Neg :: stack
    | _ -> Pos :: stack )
  | I64_const i -> (
    match i with
    | 0L -> Zero :: stack
    | i when Int64.lt i 0L -> Neg :: stack
    | _ -> Pos :: stack )
  (* | F32_const f -> *)
  (* | F64_const f ->  *)
  | Unreachable -> Fmt.failwith "unreachable"
  | I_unop (_nn, op) -> exec_iunop stack op
  (* | If_else (_, _, expr_true, expr_false) -> *)
  (*   let stack = match stack with [] -> assert false | _ :: t -> t in *)
  (*   let _stack_true = expr stack expr_true.raw in *)
  (*   let _stack_false = expr stack expr_false.raw in *)
  (*   [] *)
  | Return -> (
    match stack with
    | [] -> Fmt.failwith "stack empty on return"
    | h :: t ->
      Fmt.pr "return %a" Sign.pp h;
      t )
  | instr -> Fmt.failwith "Instr unimplemented %a" (pp_instr ~short:true) instr

and expr stack (expr : expr) : value list =
  List.fold_left
    (fun acc (x : instr Annotated.t) -> exec_instr acc x.raw)
    stack expr

let expr e =
  let _ = expr [] (Annotated.raw e) in
  ()
