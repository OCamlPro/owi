(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2025 OCamlPro *)
(* Written by the Owi programmers *)

module type VariableType = sig
  type t

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

module type S = sig
  type 'a t

  type key

  val print : 'a Fmt.t -> 'a t Fmt.t

  val empty : 'a t

  (** [add ~merge key value uf] adds the key [key] with associated value [value]
      to the union-find.

      If [key] is already present in the union-find (including if it is no
      longer canonical), [merge] is used to combine the new value with the
      existing value associated with [key]. *)
  val add : merge:('a -> 'a -> 'a) -> key -> 'a -> 'a t -> 'a t

  (** [find_canonical key uf] returns the current canonical representative for
      [key]. *)
  val find_canonical : key -> 'a t -> key

  (** [find_opt key uf] returns the value associated with [key], if any.

      [key] does not need to be canonical. *)
  val find_opt : key -> 'a t -> 'a option

  (** [union ~merge key1 key2 uf] merges the equivalence classes associated with
      [key1] and [key2], calling [merge] on the corresponding values. *)
  val union : merge:('a -> 'a -> 'a) -> key -> key -> 'a t -> 'a t

  val explode : 'a t -> 'a list
end

module Make (X : VariableType) : S with type key = X.t = struct
  module MX = Map.Make (X)
  module SX = Set.Make (X)

  type key = X.t

  type 'a node =
    { aliases : SX.t
    ; cardinal : int
    ; datum : 'a option
    }

  type 'a t =
    { canonical_elements : X.t MX.t
    ; node_of_canonicals : 'a node MX.t
    }

  let print_set ppf set =
    if SX.is_empty set then Fmt.pf ppf "{}"
    else (
      Fmt.pf ppf "@[<hov 1>{";
      let first = ref true in
      SX.iter
        (fun x ->
          if !first then first := false else Fmt.pf ppf ",@ ";
          X.pp ppf x )
        set;
      Fmt.pf ppf "}@]" )

  let print_map pp ppf map =
    if MX.is_empty map then Fmt.pf ppf "{}"
    else (
      Fmt.pf ppf "@[<hov 1>{";
      let first = ref true in
      MX.iter
        (fun key value ->
          if !first then first := false else Fmt.pf ppf ",@ ";
          Fmt.pf ppf "@[<hov 1>(%a@ %a)@]" X.pp key pp value )
        map;
      Fmt.pf ppf "}@]" )

  let print_aliases ppf { aliases; _ } = print_set ppf aliases

  let print_datum pp ppf { datum; _ } =
    Fmt.option ~none:(fun ppf () -> Fmt.pf ppf "<default>") pp ppf datum

  let[@ocamlformat "disable"] print pp ppf { node_of_canonicals; _ } =
    Fmt.pf ppf
      "@[<hov 1>(\
       @[<hov 1>(aliases_of_canonicals@ %a)@]@ \
       @[<hov 1>(payload_of_canonicals@ %a)@]\
       )@]"
      (print_map print_aliases) node_of_canonicals
      (print_map (print_datum pp)) node_of_canonicals

  let empty = { canonical_elements = MX.empty; node_of_canonicals = MX.empty }

  let find_canonical variable t =
    match MX.find_opt variable t.canonical_elements with
    | None -> variable
    | Some canonical -> canonical

  let add ~merge variable datum t =
    let variable = find_canonical variable t in
    let node_of_canonicals =
      MX.update variable
        (function
          | None ->
            Some { aliases = SX.empty; cardinal = 0; datum = Some datum }
          | Some node ->
            let datum =
              match node.datum with
              | None -> Some datum
              | Some existing_datum -> Some (merge datum existing_datum)
            in
            Some { node with datum } )
        t.node_of_canonicals
    in
    { t with node_of_canonicals }

  let find_node_opt canonical t = MX.find_opt canonical t.node_of_canonicals

  let find_node canonical t =
    match find_node_opt canonical t with
    | None -> { aliases = SX.empty; cardinal = 0; datum = None }
    | Some node -> node

  let find_opt variable t =
    Option.bind
      (find_node_opt (find_canonical variable t) t)
      (fun node -> node.datum)

  let set_canonical_element aliases canonical canonical_elements =
    SX.fold
      (fun alias canonical_elements -> MX.add alias canonical canonical_elements)
      aliases canonical_elements

  let union ~merge lhs rhs t =
    let lhs = find_canonical lhs t in
    let rhs = find_canonical rhs t in
    if X.equal lhs rhs then t
    else
      let lhs_node = find_node lhs t in
      let rhs_node = find_node rhs t in
      let demoted, canonical, canonical_elements =
        if lhs_node.cardinal < rhs_node.cardinal then
          ( lhs
          , rhs
          , set_canonical_element lhs_node.aliases rhs t.canonical_elements )
        else
          ( rhs
          , lhs
          , set_canonical_element rhs_node.aliases lhs t.canonical_elements )
      in
      let datum =
        match (lhs_node.datum, rhs_node.datum) with
        | None, None -> None
        | None, Some datum | Some datum, None -> Some datum
        | Some lhs_datum, Some rhs_datum -> Some (merge lhs_datum rhs_datum)
      in
      let node =
        { aliases = SX.add demoted (SX.union lhs_node.aliases rhs_node.aliases)
        ; cardinal = lhs_node.cardinal + rhs_node.cardinal + 1
        ; datum
        }
      in
      let canonical_elements = MX.add demoted canonical canonical_elements in
      let node_of_canonicals = MX.add canonical node t.node_of_canonicals in
      let node_of_canonicals = MX.remove demoted node_of_canonicals in
      { canonical_elements; node_of_canonicals }

  let explode { canonical_elements = _; node_of_canonicals } =
    MX.to_list node_of_canonicals
    |> List.map (fun (_k, v) ->
      match v.datum with None -> assert false | Some v -> v )
end
