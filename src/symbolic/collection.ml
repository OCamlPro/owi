(** Collections. *)

module type S = sig
  type t

  type concrete

  type symbolic

  val init : unit -> t

  val clone : t -> t

  val get : int -> concrete -> t -> int -> symbolic
end

module Make (M : sig
  type concrete

  type symbolic

  val convert_one : concrete -> symbolic

  val clone_one : symbolic -> symbolic
end) =
struct
  type t = (int * int, M.symbolic) Hashtbl.t

  let init () = Hashtbl.create 16

  let clone collection =
    let collection' = init () in
    Hashtbl.iter
      (fun loc memory ->
        let memory = M.clone_one memory in
        Hashtbl.add collection' loc memory )
      collection;
    collection'

  let get env_id (original : M.concrete) collection id =
    let loc = (env_id, id) in
    match Hashtbl.find_opt collection loc with
    | None ->
      let g = M.convert_one original in
      Hashtbl.add collection loc g;
      g
    | Some t -> t
end
