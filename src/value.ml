
module Func = struct
  type func_id = Fid of int [@@unboxed]

  type 'value t =
    | WASM of func_id * Simplify_bis.func
    | Extern of Types.func_type * ('value list -> 'value list)

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      Fid !r

  let wasm func : _ t = WASM (fresh (), func)

  let type_ = function
    | WASM (_, func) -> func.type_f
    | Extern (type_, _) -> type_
end

type externref = ..

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of ref_value

and ref_value =
  | Externref of externref option
  | Funcref of func option

and func = t Func.t

let pp_ref fmt = function
  | Externref _ -> Format.fprintf fmt "externref"
  | Funcref _ -> Format.fprintf fmt "funcref"

let pp fmt = function
  | I32 i -> Format.fprintf fmt "i32.const %ld" i
  | I64 i -> Format.fprintf fmt "i64.const %Ld" i
  | F32 f -> Format.fprintf fmt "f32.const %a" Pp.Simplified.f32 f
  | F64 f -> Format.fprintf fmt "f64.const %a" Pp.Simplified.f64 f
  | Ref r -> pp_ref fmt r

let ref_null (type_:Types.ref_type) =
  match type_ with
  | Func_ref -> Ref (Funcref None)
  | Extern_ref -> Ref (Externref None)
