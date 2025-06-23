type 'a t = { raw : 'a }

let dummy raw = { raw }

let dummies l = List.map (fun raw -> { raw }) l

let dummy_deep raw =
  let raw = dummies raw in
  { raw }

let map f { raw } =
  let raw = f raw in
  { raw }

let iter f { raw } = f raw
