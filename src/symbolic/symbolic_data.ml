type t = Concrete_data.t

let value data = data.Concrete_data.value

let size data = String.length data.Concrete_data.value

let drop data = data.Concrete_data.value <- ""
