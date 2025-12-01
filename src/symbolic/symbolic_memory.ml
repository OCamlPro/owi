include Symbolic_memory_concretizing

let load_8_s m a = Symbolic_choice_with_memory.lift_mem @@ load_8_s m a

let load_8_u m a = Symbolic_choice_with_memory.lift_mem @@ load_8_u m a

let load_16_s m a = Symbolic_choice_with_memory.lift_mem @@ load_16_s m a

let load_16_u m a = Symbolic_choice_with_memory.lift_mem @@ load_16_u m a

let load_32 m a = Symbolic_choice_with_memory.lift_mem @@ load_32 m a

let load_64 m a = Symbolic_choice_with_memory.lift_mem @@ load_64 m a

let store_8 m ~addr v =
  Symbolic_choice_with_memory.lift_mem @@ store_8 m ~addr v

let store_16 m ~addr v =
  Symbolic_choice_with_memory.lift_mem @@ store_16 m ~addr v

let store_32 m ~addr v =
  Symbolic_choice_with_memory.lift_mem @@ store_32 m ~addr v

let store_64 m ~addr v =
  Symbolic_choice_with_memory.lift_mem @@ store_64 m ~addr v

let fill m ~pos ~len c =
  Symbolic_choice_with_memory.lift_mem @@ fill m ~pos ~len c

let blit m1 ~src m2 ~dst ~len =
  Symbolic_choice_with_memory.lift_mem @@ blit m1 ~src m2 ~dst ~len
