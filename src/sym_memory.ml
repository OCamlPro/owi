let ( let* ) o f = Option.bind o f

let ( let+ ) o f = Option.map f o

let return x = Option.Some x

module M = struct
  module Expr = Encoding.Expression
  open Expr

  type int32 = Expr.t

  type int64 = Expr.t

  type t =
    { map : (Int32.t, Expr.t) Hashtbl.t
    ; parent : t Option.t
    ; size : Int32.t
    }

  let create size = { map = Hashtbl.create 128; parent = None; size }

  let grow _m _size = assert false

  let size (m : t) : int32 = Val (Num (I32 m.size))

  let clone (m : t) : t =
    { map = Hashtbl.create 0; parent = Some m; size = m.size }

  let size_in_pages (_m : t) : int32 = assert false

  let concretize (addr : int32) : Int32.t =
    match addr with Val (Num (I32 i)) -> i | _ -> assert false

  let rec load_byte (a : Int32.t) (m : t) : Expr.t Option.t =
    match Hashtbl.find_opt m.map a with
    | Some b -> Some b
    | None -> Option.bind m.parent (load_byte a)

  let load_8_s (m : t) (a : int32) : int32 =
    let b8 = load_byte (concretize a) m in
    Option.fold b8 ~none:(Val (Num (I32 0l))) ~some:(fun n ->
      match n with
      | Val (Num (I32 i)) -> Val (Num (I32 (Int32.extend_s 8 i)))
      | e -> Extract (Val (Num (I32 0l)), 3, 0) ++ e)

  let load_8_u (m : t) (a : Expr.t) : Expr.t =
    let b8 = load_byte (concretize a) m in
    Option.fold b8 ~none:(Val (Num (I32 0l))) ~some:(fun n ->
        match n with
        | Val (Num (I32 _)) as n' -> n'
        | e -> Extract (Val (Num (I32 0l)), 3, 0) ++ e)

  let load_16_s (mem : t) (a : Expr.t) : Expr.t =
    let b16 =
      let a' = concretize a in
      let* b1 = load_byte a' mem in
      let* b2 = load_byte (Int32.add a' 1l) mem in
      return (Extract (Val (Num (I32 0l)), 2, 0) ++ b1 ++ b2)
    in
    Option.value b16 ~default:(Val (Num (I32 0l)))

  let load_16_u (mem : t) (a : Expr.t) : Expr.t =
    (* Wrong *)
    load_16_s mem a

  let load_32 (_mem : t) (_a : Expr.t) : Expr.t = assert false

  let load_64 (_mem : t) (_a : Expr.t) : Expr.t = assert false

  let extract (v : int32) (h : int) (l : int) : int32 =
    match v with
    | Val (Num (I32 i)) ->
        let i' = Int32.(of_int (l * 8) |> shr_s i |> logand 0xffl) in
        Val (Num (I32 i'))
    | Val (Num (I64 i)) ->
        let i' = Int64.(of_int (l * 8) |> shr_s i |> logand 0xffL) in
        Val (Num (I32 (Int32.of_int64 i')))
    | v' -> Extract (v', h, l)

  let storen (m : t) ~(addr : int32) (v : int32) (n : int) : unit =
    let a0 = concretize addr in
    for i = 0 to n - 1 do
      let addr' = Int32.add a0 (Int32.of_int i)
      and v' = extract v (i + 1) i in
      Hashtbl.replace m.map addr' v'
    done

  let store_8 (m : t) ~(addr : int32) (v : int32) : unit = storen m ~addr v 1

  let store_16 (m : t) ~(addr : int32) (v : int32) : unit = storen m ~addr v 2

  let store_32 (m : t) ~(addr : int32) (v : int32) : unit = storen m ~addr v 4

  let store_64 (m : t) ~(addr : int32) (v : int64) : unit = storen m ~addr v 8
end

module M' : Interpret_functor_intf.Memory_data = M

let memory = M.create 65536l
