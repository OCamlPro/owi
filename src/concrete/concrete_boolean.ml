type t = bool

let false_ = false

let true_ = true

let of_concrete c = c

let not = not

let and_ = ( && )

let or_ = ( || )

let to_i32 = function false -> 0l | true -> 1l

let pp = Fmt.bool
