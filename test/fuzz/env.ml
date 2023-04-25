open Owi.Types.Symbolic
open Crowbar

type t =
  { next_global : int
  ; next_fun : int
  ; next_local : int
  ; globals_i32 : string list
  ; globals_i64 : string list
  ; funcs : (string * block_type) list
  }

let empty =
  { next_global = 0
  ; next_fun = 0
  ; next_local = 0
  ; globals_i32 = []
  ; globals_i64 = []
  ; funcs = []
  }

let v = ref empty

let reset () = v := empty

let add_global_i32 () =
  let n = !v.next_global in
  let name = Format.sprintf "g%d" n in
  v := { !v with next_global = succ n; globals_i32 = name :: !v.globals_i32 };
  name

let add_global_i64 () =
  let n = !v.next_global in
  let name = Format.sprintf "g%d" n in
  v := { !v with next_global = succ n; globals_i64 = name :: !v.globals_i64 };
  name

let add_func typ =
  let n = !v.next_fun in
  let name = Format.sprintf "f%d" n in
  v := { !v with next_fun = succ n; funcs = (name, typ) :: !v.funcs };
  name
