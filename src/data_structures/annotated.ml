module S = Set.Make (Int)

type 'a t =
  { raw : 'a
  ; instr_counter : int Atomic.t
  ; mutable functions_called : S.t
  ; mutable distances : int array
  ; mutable d_true : int array
  ; mutable d_false : int array
  }

let dummy raw =
  { raw
  ; instr_counter = Atomic.make 0
  ; functions_called = S.empty
  ; distances = Array.make 0 0
  ; d_true = Array.make 0 0
  ; d_false = Array.make 0 0
  }

let dummies l =
  List.map
    (fun raw ->
      { raw
      ; instr_counter = Atomic.make 0
      ; functions_called = S.empty
      ; distances = Array.make 0 0
      ; d_true = Array.make 0 0
      ; d_false = Array.make 0 0
      } )
    l

let dummy_deep raw =
  let raw = dummies raw in
  { raw
  ; instr_counter = Atomic.make 0
  ; functions_called = S.empty
  ; distances = Array.make 0 0
  ; d_true = Array.make 0 0
  ; d_false = Array.make 0 0
  }

let map f { raw; instr_counter; functions_called; distances; d_true; d_false } =
  let raw = f raw in
  { raw; instr_counter; functions_called; distances; d_true; d_false }

let iter f { raw; _ } = f raw

let update_functions_called annot l = annot.functions_called <- l

let init_distances annot a = annot.distances <- a

let init_d_true annot a = annot.d_true <- a

let init_d_false annot a = annot.d_false <- a
