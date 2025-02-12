(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include
  Symbolic_functor.Make [@inlined hint]
    (Symbolic_memory_concretizing)
    (Thread_with_memory)
    (Minimalist_symbolic_choice)
