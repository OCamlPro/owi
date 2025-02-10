module type VariableType = sig
  type t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

module type S = sig
  type 'a t

  type key

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val find_canonical : key -> 'a t -> key

  val find_opt : key -> 'a t -> 'a option

  (* Raises [Invalid_argument] if either key is not in the map. *)
  val union : merge:('a -> 'a -> 'a) -> key -> key -> 'a t -> 'a t
end

module Make (X : VariableType) : S with type key = X.t = struct
  module MX = Map.Make (X)
  module SX = Set.Make (X)

  type key = X.t

  type 'a node =
    { aliases : SX.t;
      cardinal : int;
      datum : 'a
    }

  type 'a t =
    { canonical_elements : X.t MX.t;
      node_of_canonicals : 'a node MX.t
    }

  let print_set ppf set =
    if SX.is_empty set
    then Format.fprintf ppf "{}"
    else (
      Format.fprintf ppf "@[<hov 1>{";
      let first = ref true in
      SX.iter
        (fun x ->
          if !first then first := false else Format.fprintf ppf ",@ ";
          X.print ppf x)
        set;
      Format.fprintf ppf "}@]")

  let print_map pp ppf map =
    if MX.is_empty map
    then Format.fprintf ppf "{}"
    else (
      Format.fprintf ppf "@[<hov 1>{";
      let first = ref true in
      MX.iter
        (fun key value ->
          if !first then first := false else Format.fprintf ppf ",@ ";
          Format.fprintf ppf "@[<hov 1>(%a@ %a)@]" X.print key pp value)
        map;
      Format.fprintf ppf "}@]")

  let print_aliases ppf { aliases; _ } = print_set ppf aliases

  let print_datum pp ppf { datum; _ } = pp ppf datum

  let[@ocamlformat "disable"] print pp ppf { node_of_canonicals; _ } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(aliases_of_canonicals@ %a)@]@ \
        @[<hov 1>(payload_of_canonicals@ %a)@]\
      )@]"
      (print_map print_aliases) node_of_canonicals
      (print_map (print_datum pp)) node_of_canonicals

  let empty = { canonical_elements = MX.empty; node_of_canonicals = MX.empty }

  let add variable datum t =
    let node_of_canonicals =
      MX.add variable
        { aliases = SX.empty; cardinal = 0; datum }
        t.node_of_canonicals
    in
    { t with node_of_canonicals }

  let find_canonical variable t =
    match MX.find_opt variable t.canonical_elements with
    | None -> variable
    | Some canonical -> canonical

  let find_node_opt canonical t = MX.find_opt canonical t.node_of_canonicals

  let find_node_or_raise name canonical t =
    match find_node_opt canonical t with
    | None -> invalid_arg name
    | Some node -> node

  let find_opt variable t =
    Option.map
      (fun node -> node.datum)
      (find_node_opt (find_canonical variable t) t)

  let set_canonical_element aliases canonical canonical_elements =
    SX.fold
      (fun alias canonical_elements ->
        MX.add alias canonical canonical_elements)
      aliases canonical_elements

  let union ~merge lhs rhs t =
    let lhs = find_canonical lhs t in
    let rhs = find_canonical rhs t in
    if X.equal lhs rhs
    then t
    else
      let lhs_node = find_node_or_raise "union" lhs t in
      let rhs_node = find_node_or_raise "union" rhs t in
      let demoted, canonical, canonical_elements =
        if lhs_node.cardinal < rhs_node.cardinal
        then
          ( lhs,
            rhs,
            set_canonical_element lhs_node.aliases rhs t.canonical_elements )
        else
          ( rhs,
            lhs,
            set_canonical_element rhs_node.aliases lhs t.canonical_elements )
      in
      let node =
        { aliases = SX.union lhs_node.aliases rhs_node.aliases;
          cardinal = lhs_node.cardinal + rhs_node.cardinal + 1;
          datum = merge lhs_node.datum rhs_node.datum
        }
      in
      let node_of_canonicals = MX.add canonical node t.node_of_canonicals in
      let node_of_canonicals = MX.remove demoted node_of_canonicals in
      { canonical_elements; node_of_canonicals }
end
