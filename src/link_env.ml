open Syntax
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module IMap = Map.Make (Int)

type data = { mutable value : string }

let drop_data data = data.value <- ""

type elem = { mutable value : Value.ref_value array }

let drop_elem elem = elem.value <- [||]

type extern_funcs = Value.Func.extern_func Func_id.collection

type t' = Env_id.t

type 'ext t =
  { globals : Global.t IMap.t
  ; memories : Memory.t IMap.t
  ; tables : Table.t IMap.t
  ; functions : Func_intf.t IMap.t
  ; data : data IMap.t
  ; elem : elem IMap.t
  ; extern_funcs : 'ext Func_id.collection
  }

let pp fmt t =
  let global fmt (id, (global : Global.t)) =
    Format.fprintf fmt "%a -> %a" Format.pp_print_int id Value.pp global.value
  in
  let func fmt (id, (_func : Value.Func.t)) =
    Format.fprintf fmt "%a -> func" Format.pp_print_int id
  in
  Format.fprintf fmt "@[<hov 2>{@ (globals %a)@ (functions %a)@ }@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       global )
    (IMap.bindings t.globals)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       func )
    (IMap.bindings t.functions)

let get_global (env : _ t) id =
  match IMap.find_opt id env.globals with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown global"
  | Some v -> Ok v

let get_memory (env : _ t) id =
  match IMap.find_opt id env.memories with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown memory"
  | Some v -> Ok v

let get_table (env : _ t) id =
  match IMap.find_opt id env.tables with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown table"
  | Some v -> Ok v

let get_func (env : _ t) id =
  match IMap.find_opt id env.functions with
  | None ->
    Log.debug "%a@." pp env;
    error_s "unknown function %a" Format.pp_print_int id
  | Some v -> Ok v

let get_data (env : _ t) id =
  match IMap.find_opt id env.data with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown data"
  | Some v -> Ok v

let get_elem (env : _ t) id =
  match IMap.find_opt id env.elem with
  | None ->
    Log.debug "%a@." pp env;
    Error "unknown elem"
  | Some v -> Ok v

let get_extern_func env id = Func_id.get id env.extern_funcs

let get_func_typ env f =
  match f with
  | Func_intf.WASM (_, func, _) -> func.type_f
  | Extern id -> Func_id.get_typ id env.extern_funcs

module Build = struct
  type t =
    { globals : Global.t IMap.t
    ; memories : Memory.t IMap.t
    ; tables : Table.t IMap.t
    ; functions : Func_intf.t IMap.t
    ; data : data IMap.t
    ; elem : elem IMap.t
    }

  let empty =
    { globals = IMap.empty
    ; memories = IMap.empty
    ; tables = IMap.empty
    ; functions = IMap.empty
    ; data = IMap.empty
    ; elem = IMap.empty
    }

  let add_global id const env =
    { env with globals = IMap.add id const env.globals }

  let add_memory id mem env =
    { env with memories = IMap.add id mem env.memories }

  let add_table id table env =
    { env with tables = IMap.add id table env.tables }

  let add_func id func env =
    { env with functions = IMap.add id func env.functions }

  let add_data id data env = { env with data = IMap.add id data env.data }

  let add_elem id elem env = { env with elem = IMap.add id elem env.elem }

  let get_global (env : t) id =
    match IMap.find_opt id env.globals with
    | None ->
      (* Log.debug "%a@." pp env; *)
      Error "unknown global"
    | Some v -> Ok v

  let get_const_global (env : t) id =
    let* g = get_global env id in
    match g.mut with
    | Const -> ok g.value
    | Var -> Error "constant expression required"

  let get_func (env : t) id =
    match IMap.find_opt id env.functions with
    | None ->
      (* Log.debug "%a@." pp env; *)
      error_s "unknown function %a" Format.pp_print_int id
    | Some v -> Ok v
end

module type T = sig
  module V : Intf.V

  type extern_func

  type t

  type t' = t Lazy.t

  type elem = { mutable value : Value.ref_value array }

  type data = { mutable value : string }

  type func := Func_intf.t

  val get_memory : t -> int -> Memory.t Result.t

  val get_func : t -> int -> func Result.t

  val get_table : t -> int -> Table.t Result.t

  val get_elem : t -> int -> elem Result.t

  val get_data : t -> int -> data Result.t

  val get_global : t -> int -> Global.t Result.t

  val drop_elem : elem -> unit

  val drop_data : data -> unit

  val get_extern_func : t -> Func_id.t -> Value.Func.extern_func

  val get_func_typ : t -> func -> Simplified.func_type

  val pp : Format.formatter -> t -> unit

  val freeze : Build.t -> extern_func Func_id.collection -> t
end

module type P = sig
  module V : Intf.V

  val const_i32 : Int32.t -> V.int32

  val const_i64 : Int64.t -> V.int64

  val const_f32 : Float32.t -> V.float32

  val const_f64 : Float64.t -> V.float64
end

let freeze Build.{ globals; memories; tables; functions; data; elem }
  extern_funcs =
  { globals; memories; tables; functions; data; elem; extern_funcs }
