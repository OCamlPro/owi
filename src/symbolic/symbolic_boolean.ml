include Smtml.Typed.Bool

let of_concrete (i : bool) : t = if i then true_ else false_ [@@inline]
