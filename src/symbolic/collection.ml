(** Collections. *)

module type S = sig
  type t

  type symbolic

  val empty : t

  val clone : t -> t

  val find : t -> env_id:int -> id:int -> symbolic option

  val replace : t -> env_id:int -> id:int -> symbolic -> t
end

module Make (M : sig
  type symbolic

  val clone_one : symbolic -> symbolic
end) =
struct
  module Int_pair = struct
    type t = int * int

    let compare (l1, r1) (l2, r2) =
      let res = compare l1 l2 in
      if l1 = l2 then compare r1 r2 else res
  end

  module IPMap = Map.Make (Int_pair)

  type t = M.symbolic IPMap.t

  let empty = IPMap.empty

  let clone collection = IPMap.map M.clone_one collection

  let find collection ~env_id ~id =
    let loc = (env_id, id) in
    IPMap.find_opt loc collection

  let replace collection ~env_id ~id v =
    let loc = (env_id, id) in
    IPMap.add loc v collection
end
