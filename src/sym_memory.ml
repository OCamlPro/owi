let ( let* ) = Option.bind

let ( let+ ) o f = Option.map f o

module Memory = struct
  module Expr = Encoding.Expression

  open Expr

  type int32 = Expr.t

  type int64 = Expr.t

  type t =
    { map : (Int32.t, Expr.t) Hashtbl.t
    ; parent : t Option.t
    }

  let create _ = { map = Hashtbl.create 128; parent = None }

  let grow m _size = m

  let size (m : t) : int32 =
  Val (Num (I32 (Int32.of_int (Hashtbl.length m.map))))

  let size_in_pages (m : t) : int32 = size m

  let concretize_addr (a : int32) : Int32.t =
    match a with Val (Num (I32 a')) -> a' | _ -> assert false

  let rec load_byte_rec (a : Int32.t) (m: t) : Expr.t Option.t =
    match Hashtbl.find_opt m.map a with
    | Some b -> Some b
    | None -> Option.bind m.parent (load_byte_rec a)

  let load_8_s (m : t) (a : int32) : int32 =
    match load_byte_rec (concretize_addr a) m with
    | None -> Val (Num (I32 0l))
    | Some n -> (
        match n with
        | Val (Num (I32 n)) -> Val (Num (I32 (Int32.extend_s 8 n)))
        | e -> Extract (Val (Num (I64 0L)), 3, 0) ++ e)

  let load_8_u (mem : t) (a : Expr.t) : Expr.t =
    (* Wrong *)
    load_8_s mem a

  let load_16_s (mem : t) (a : Expr.t) : Expr.t =
    let res =
      let a' = concretize_addr a in
      let* b1 = load_byte_rec a' mem in
      let+ b2 = load_byte_rec (Int32.add a' 1l) mem in
      Extract (Val (Num (I64 0L)), 2, 0) ++ b1 ++ b2
    in
    Option.get res

  let load_16_u (mem : t) (a : Expr.t) : Expr.t =
    (* Wrong *)
    load_16_s mem a

  let load_32 (_mem : t) (_a : Expr.t) : Expr.t = assert false

  let load_64 (_mem : t) (_a : Expr.t) : Expr.t = assert false

  let storen (m : t) ~(addr : int32) (v : int32) (n : int) : unit =
    let a0 = concretize_addr addr in
    for i=0 to (n - 1) do
      Hashtbl.replace m.map (Int32.add a0 (Int32.of_int i))
        (Extract (v, i + 1, i))
    done

  let store_8 (m: t) ~(addr : int32) (v : int32) : unit = storen m ~addr v 1

  let store_16 (m : t) ~(addr : int32) (v : int32) : unit = storen m ~addr v 2

  let store_32 (m : t) ~(addr : int32) (v : int32) : unit = storen m ~addr v 4

  let store_64 (m : t) ~(addr : int32) (v : int64) : unit = storen m ~addr v 8
end

let mem = Memory.create 0l
