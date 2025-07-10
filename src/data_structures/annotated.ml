type 'a t =
  { raw : 'a
  ; instr_counter : int Atomic.t
  }

let dummy raw = { raw; instr_counter = Atomic.make 0 }

let dummies l = List.map (fun raw -> { raw; instr_counter = Atomic.make 0 }) l

let dummy_deep raw =
  let raw = dummies raw in
  { raw; instr_counter = Atomic.make 0 }

let map f { raw; instr_counter } =
  let raw = f raw in
  { raw; instr_counter }

let iter f { raw; _ } = f raw
