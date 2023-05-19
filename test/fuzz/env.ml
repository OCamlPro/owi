open Owi.Types.Symbolic
open Crowbar

type t =
  { next_global : int
  ; next_fun : int
  ; next_local : int
  ; globals : (string * global_type) list
  ; locals : param list
  ; funcs : (string * block_type) list
  ; fuel : int
  }

let empty =
  { next_global = 0
  ; next_fun = 0
  ; next_local = 0
  ; globals = []
  ; locals = []
  ; funcs = []
  ; fuel = Param.initial_fuel
  }

let v = ref empty

let reset () = v := empty

let reset_locals () = v := { !v with locals = [] }

let add_global typ =
  let n = !v.next_global in
  let name = Format.sprintf "g%d" n in
  let globals = (name, typ) :: !v.globals in
  let next_global = succ n in
  v := { !v with next_global; globals };
  name

let add_local typ =
  let n = !v.next_local in
  let name = Format.sprintf "l%d" n in
  let locals = (Some name, typ) :: !v.locals in
  let next_local = succ n in
  v := { !v with next_local; locals };
  name

let add_func typ =
  let n = !v.next_fun in
  let name = Format.sprintf "f%d" n in
  v := { !v with next_fun = succ n; funcs = (name, typ) :: !v.funcs };
  name

let use_fuel () = v := { !v with fuel = pred !v.fuel }

let has_fuel () = !v.fuel > 0

let has_no_fuel () = not (has_fuel ())

let refill_fuel () = v := { !v with fuel = Param.initial_fuel }
