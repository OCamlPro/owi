type 'a t =
  { raw : 'a
  ; instr_counter : int Atomic.t
  ; mutable functions_called : int list
  }

let dummy raw = { raw; instr_counter = Atomic.make 0; functions_called = [] }

let dummies l =
  List.map
    (fun raw -> { raw; instr_counter = Atomic.make 0; functions_called = [] })
    l

let dummy_deep raw =
  let raw = dummies raw in
  { raw; instr_counter = Atomic.make 0; functions_called = [] }

let map f { raw; instr_counter; functions_called } =
  let raw = f raw in
  { raw; instr_counter; functions_called }

let iter f { raw; _ } = f raw

let update_functions_called ann l = ann.functions_called <- l
