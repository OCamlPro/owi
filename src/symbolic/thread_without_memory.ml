include Thread.Make (struct
  type collection = unit

  let init () = ()

  let clone () = ()
end)
