(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Link_env

type t = Symbolic_extern_func.extern_func Link_env.t

let get_memory env id =
  match get_memory env id with
  | Error e -> Symbolic_choice.trap e
  | Ok orig_mem ->
    let f (t : Thread.t) =
      let memories = Thread.memories t in
      Symbolic_memory_collection.get_memory (Link_env.id env) orig_mem memories
        id
    in
    Symbolic_choice.with_thread f

let get_table (env : t) i : Symbolic_table.t Symbolic_choice.t =
  match get_table env i with
  | Error e -> Symbolic_choice.trap e
  | Ok orig_table ->
    let f (t : Thread.t) =
      let tables = Thread.tables t in
      Symbolic_table.get_table (Link_env.id env) orig_table tables i
    in
    Symbolic_choice.with_thread f

let get_data env n =
  match get_data env n with
  | Error e -> Symbolic_choice.trap e
  | Ok orig_data -> Symbolic_choice.return orig_data

let get_global (env : t) i : Symbolic_global.t Symbolic_choice.t =
  match get_global env i with
  | Error e -> Symbolic_choice.trap e
  | Ok orig_global ->
    let f (t : Thread.t) =
      let globals = Thread.globals t in
      Symbolic_global.get_global (Link_env.id env) orig_global globals i
    in
    Symbolic_choice.with_thread f
