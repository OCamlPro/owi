open Owi.Types.Symbolic
open Crowbar

type t =
  { mutable next_global : int
  ; mutable next_fun : int
  ; mutable next_local : int
  ; mutable globals : (string * global_type) list
  ; mutable locals : param list
  ; mutable funcs : (string * block_type) list
  ; mutable fuel : int
  }

let empty () =
  { next_global = 0
  ; next_fun = 0
  ; next_local = 0
  ; globals = []
  ; locals = []
  ; funcs = []
  ; fuel = Param.initial_fuel
  }

let reset_locals env = env.locals <- []

let add_global env typ =
  let n = env.next_global in
  let name = Format.sprintf "g%d" n in
  env.globals <- (name, typ) :: env.globals;
  env.next_global <- succ n;
  name

let add_local env typ =
  let n = env.next_local in
  let name = Format.sprintf "l%d" n in
  env.locals <- (Some name, typ) :: env.locals;
  env.next_local <- succ n;
  name

let add_func env typ =
  let n = env.next_fun in
  let name = Format.sprintf "f%d" n in
  env.next_fun <- succ n;
  env.funcs <- (name, typ) :: env.funcs;
  name

let use_fuel env = env.fuel <- pred env.fuel

let has_fuel env = env.fuel > 0

let has_no_fuel env = not (has_fuel env)

let refill_fuel env = env.fuel <- Param.initial_fuel
