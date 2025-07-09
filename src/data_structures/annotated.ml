type 'a t =
  { raw : 'a
  ; nb_iter : int Atomic.t
  }

let dummy raw = { raw; nb_iter = Atomic.make 0 }

let dummies l = List.map (fun raw -> { raw; nb_iter = Atomic.make 0 }) l

let dummy_deep raw =
  let raw = dummies raw in
  { raw; nb_iter = Atomic.make 0 }

let map f { raw; nb_iter } =
  let raw = f raw in
  { raw; nb_iter }

let iter f { raw; _ } = f raw
