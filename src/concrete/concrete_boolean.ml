type t = bool

let false_ = false

let true_ = true

let[@inline] to_bool b = b

let[@inline] of_bool c = c

let[@inline] not b = not b

let[@inline] and_ x y = x && y

let[@inline] or_ x y = x || y

let[@inline] pp fmt b = Fmt.bool fmt b
