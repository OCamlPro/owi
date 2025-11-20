module S = Set.Make (Int)

type 'a t =
  { raw : 'a
  ; instr_counter : int Atomic.t
  ; mutable functions_called : S.t
  ; d_true : int array option ref
  ; d_false : int array option ref
  }

let raw { raw; _ } = raw

let dummy raw =
  { raw
  ; instr_counter = Atomic.make 0
  ; functions_called = S.empty
  ; d_true = ref None
  ; d_false = ref None
  }

let dummies l =
  List.map
    (fun raw ->
      { raw
      ; instr_counter = Atomic.make 0
      ; functions_called = S.empty
      ; d_true = ref None
      ; d_false = ref None
      } )
    l

let dummy_deep raw =
  let raw = dummies raw in
  { raw
  ; instr_counter = Atomic.make 0
  ; functions_called = S.empty
  ; d_true = ref None
  ; d_false = ref None
  }

let map f { raw; instr_counter; functions_called; d_true; d_false } =
  let raw = f raw in
  { raw; instr_counter; functions_called; d_true; d_false }

let iter f { raw; _ } = f raw

let update_functions_called annot l = annot.functions_called <- l

let set_d_true annot a =
  match !(annot.d_true) with None -> annot.d_true := Some a | _ -> ()

let set_d_false annot a =
  match !(annot.d_false) with None -> annot.d_false := Some a | _ -> ()
