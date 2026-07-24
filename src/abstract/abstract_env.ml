(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type empty = |

include Link.Make (struct
  type extern_func = Abstract_extern.Func.t

  type extern_module = Abstract_extern.Module.t

  let to_func_type = Abstract_extern.Func.to_func_type

  type data = (* TODO *) string

  let data_of_concrete _ = assert false
end)

let get_memory ~modul:_ _env _id = assert false

let get_table ~modul:_ _env _id = assert false

let get_elem ~modul:_ _env _id = assert false

let get_global ~modul:_ _env _id = assert false
