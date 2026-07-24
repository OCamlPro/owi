(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type empty = |

include
  Env_intf.T
    with type memory := (* TODO*) Concrete_memory.t
     and type data := (* TODO *) string
     and type global := (* TODO *) empty
     and type elem := (* TODO *) empty
     and type table := (* TODO *) empty
     and type extern_func := Abstract_extern.Func.t
     and type extern_module := Abstract_extern.Module.t
     and type 'a choice := 'a Abstract_monad.t
