open Encoding
open Expression

type size = int32
type address = int32
type offset = int32



module Memory = struct 
  type t = { map : (address, Expression.t) Hashtbl.t; parent : t Option.t }

  let (let*) = Option.bind 
  let (let+) o f = Option.map f o  

  let concretize_addr (addr : Expression.t) : address = 
    match addr with 
    | Val (Num (I32 a)) -> a 
    | _ -> assert false 

  let rec load_byte_rec (a : address) (lmem : t) : Expression.t Option.t =
    match Hashtbl.find_opt lmem.map a with
    | Some b -> Some b
    | None -> Option.bind lmem.parent (load_byte_rec a)
   
  let load_8_s (mem : t) (a : Expression.t) : Expression.t = 
    match load_byte_rec (concretize_addr a) mem with 
    | None -> assert false 
    | Some b -> (Extract (Val (Num (I64 0L)), 3, 0)) ++ b
  
  let load_8_u (mem : t) (a : Expression.t) : Expression.t = 
    load_8_s mem a  
  
  let load_16_s (mem : t) (a : Expression.t) : Expression.t = 
    let res = 
      let a' = concretize_addr a in 
      let* b1 = load_byte_rec a' mem in 
      let+ b2 = load_byte_rec (Int32.add a' 1l) mem in 
      ((Extract (Val (Num (I64 0L)), 3, 0) ++ b1) ++ b2) in 
    Option.get res 

  let load_16_u (mem : t) (a : Expression.t) : Expression.t = 
    load_16_s mem a 
  
  let load_32 (mem : t) (a : Expression.t) : Expression.t = 
    assert false 

  let load_64 (mem : t) (a : Expression.t) : Expression.t = 
    assert false 

  let store_8 (mem: t) ~(addr : Expression.t) (v : Expression.t) : unit = 
    let a' = concretize_addr addr in
    let v' = Extract (v, 1, 0) in 
    Hashtbl.replace mem.map a' v' 

  let store_16 (mem: t) ~(addr : Expression.t) (v : Expression.t) : unit = 
    let a1 = concretize_addr addr in
    let a2 = Int32.add a1 1l in
    let v1 = Extract (v, 1, 0) in
    let v2 = Extract (v, 2, 1) in
    Hashtbl.replace mem.map a1 v1; 
    Hashtbl.replace mem.map a2 v2 
  

end 