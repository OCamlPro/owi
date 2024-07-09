module V = Symbolic_value

type 'a t = 'a

let return a = a

let bind a k = k a

let ( let* ) = bind

let map a k = k a

let ( let+ ) = map

let select _ = assert false

let select_i32 _ = assert false

let trap _ = assert false
