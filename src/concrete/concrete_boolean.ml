type t = bool

let to_bool b = b

let false_ = false

let true_ = true

let of_concrete c = c [@@inline]

let not = not

let and_ = ( && )

let or_ = ( || )

let pp = Fmt.bool
