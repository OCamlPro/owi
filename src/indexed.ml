type 'a t =
  { index : int
  ; value : 'a
  }

let has_index idx { index; _ } = idx = index

let get_at i values =
  let { value; _ } = List.find (has_index i) values in
  value

let pp f fmt v = Format.fprintf fmt "%a" f v.value
