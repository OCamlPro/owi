type 'a t = { raw : 'a }

let dummy raw = { raw }

let dummies l = List.map (fun raw -> { raw }) l
