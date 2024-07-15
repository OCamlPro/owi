include Thread.Make (struct
  type collection = bool

  let init () = false

  let clone _ = true
end)
