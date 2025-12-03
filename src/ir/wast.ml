open Fmt
open Text

let sp ppf () = Fmt.char ppf ' '

type const =
  | Const_I32 of Int32.t
  | Const_I64 of Int64.t
  | Const_F32 of Float32.t
  | Const_F64 of Float64.t
  | Const_V128 of Concrete_v128.t
  | Const_null of heap_type
  | Const_host of int
  | Const_extern of int

let pp_const fmt c =
  pf fmt "(%a)"
    (fun fmt c ->
      match c with
      | Const_I32 i -> pf fmt "i32.const %ld" i
      | Const_I64 i -> pf fmt "i64.const %Ld" i
      | Const_F32 f -> pf fmt "f32.const %a" Float32.pp f
      | Const_F64 f -> pf fmt "f64.const %a" Float64.pp f
      | Const_V128 v -> pf fmt "v128.const %a" Concrete_v128.pp v
      | Const_null rt -> pf fmt "ref.null %a" pp_heap_type rt
      | Const_host i -> pf fmt "ref.host %d" i
      | Const_extern i -> pf fmt "ref.extern %d" i )
    c

let pp_consts fmt c = list ~sep:sp pp_const fmt c

type action =
  | Invoke of string option * string * const list
  | Get of string option * string

let pp_action fmt = function
  | Invoke (mod_name, name, c) ->
    pf fmt {|(invoke%a "%s" %a)|} pp_id_opt mod_name name pp_consts c
  | Get _ -> pf fmt "<action_get TODO>"

type result_const =
  | Literal of const
  | Nan_canon of nn
  | Nan_arith of nn

let pp_result_const fmt = function
  | Literal c -> pp_const fmt c
  | Nan_canon n -> pf fmt "f%a.const nan:canonical" pp_nn n
  | Nan_arith n -> pf fmt "f%a.const nan:arithmetic" pp_nn n

type result =
  | Result_const of result_const
  | Result_extern_ref
  | Result_func_ref

let pp_result fmt = function
  | Result_const c -> pf fmt "(%a)" pp_result_const c
  | Result_func_ref | Result_extern_ref -> assert false

let pp_result_bis fmt = function
  | Result_const c -> pf fmt "%a" pp_result_const c
  | Result_extern_ref | Result_func_ref -> assert false

let pp_results fmt r = list ~sep:sp pp_result_bis fmt r

type assertion =
  | Assert_return of action * result list
  | Assert_trap of action * string
  | Assert_trap_module of Module.t * string
  | Assert_malformed of Module.t * string
  | Assert_malformed_quote of string * string
  | Assert_malformed_binary of string * string
  | Assert_invalid of Module.t * string
  | Assert_invalid_quote of string * string
  | Assert_invalid_binary of string * string
  | Assert_exhaustion of action * string
  | Assert_unlinkable of Module.t * string

let pp_assertion fmt = function
  | Assert_return (a, l) ->
    pf fmt "(assert_return %a %a)" pp_action a pp_results l
  | Assert_exhaustion (a, msg) ->
    pf fmt "(assert_exhaustion %a %s)" pp_action a msg
  | Assert_trap (a, f) -> pf fmt {|(assert_trap %a "%s")|} pp_action a f
  | Assert_trap_module (m, f) ->
    pf fmt {|(assert_trap_module %a "%s")|} Module.pp m f
  | Assert_invalid (m, msg) ->
    pf fmt "(assert_invalid@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" Module.pp m msg
  | Assert_unlinkable (m, msg) ->
    pf fmt "(assert_unlinkable@\n  @[<v>%a@]@\n  @[<v>%S@]@\n)" Module.pp m msg
  | Assert_malformed (m, msg) ->
    pf fmt "(assert_malformed (module binary@\n  @[<v>%a@])@\n  @[<v>%S@]@\n)"
      Module.pp m msg
  | Assert_malformed_quote (ls, msg) ->
    pf fmt "(assert_malformed_quote@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg
  | Assert_invalid_quote (ls, msg) ->
    pf fmt "(assert_invalid_quote@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg
  | Assert_malformed_binary (ls, msg) ->
    pf fmt "(assert_malformed_binary@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg
  | Assert_invalid_binary (ls, msg) ->
    pf fmt "(assert_invalid_binary@\n  @[<v>%S@]@\n  @[<v>%S@]@\n)" ls msg

type register = string * string option

let pp_register fmt (s, _name) = pf fmt "(register %s)" s

type cmd =
  | Quoted_module of string
  | Binary_module of string option * string
  | Text_module of Module.t
  | Assert of assertion
  | Register of string * string option
  | Action of action

let pp_cmd fmt = function
  | Quoted_module m -> pf fmt "(module %S)" m
  | Binary_module (id, m) -> Fmt.pf fmt "(module %a %S)" Text.pp_id_opt id m
  | Text_module m -> Module.pp fmt m
  | Assert a -> pp_assertion fmt a
  | Register (s, name) -> pp_register fmt (s, name)
  | Action _a -> pf fmt "<action>"

type script = cmd list

let pp_script fmt l = list ~sep:pp_newline pp_cmd fmt l
