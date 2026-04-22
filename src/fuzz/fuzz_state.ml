let model : Concrete_value.t list ref = ref []

let reset () = model := []

let brk = ref @@ Int32.of_int 0
