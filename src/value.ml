type _ externref_ty = ..

type ('a, 'b) eq = E : ('a, 'a) eq

let eq_externref_ty :
    type a b. a externref_ty -> b externref_ty -> (a, b) eq option =
 fun a b -> if Obj.magic a = Obj.magic b then Some (Obj.magic E) else None

module Func = struct
  type func_id = Fid of int [@@unboxed]

  type _ telt =
    | I32 : Int32.t telt
    | I64 : Int64.t telt
    | F32 : Float32.t telt
    | F64 : Float64.t telt
    | Externref : 'a externref_ty -> 'a telt

  type _ rtype =
    | R1 : 'a telt -> 'a rtype
    | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
    | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
    | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

  type (_, _) atype =
    | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | Res : ('r, 'r) atype

  type _ func_type = Func : ('f, 'r) atype * 'r rtype -> 'f func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func

  let elt_type (type t) (e : t telt) : Types.val_type =
    match e with
    | I32 -> Num_type I32
    | I64 -> Num_type I64
    | F32 -> Num_type F32
    | F64 -> Num_type F64
    | Externref _ -> Ref_type Extern_ref

  let res_type (type t) (r : t rtype) : Types.result_type =
    match r with
    | R1 a -> [ elt_type a ]
    | R2 (a, b) -> [ elt_type a; elt_type b ]
    | R3 (a, b, c) -> [ elt_type a; elt_type b; elt_type c ]
    | R4 (a, b, c, d) -> [ elt_type a; elt_type b; elt_type c; elt_type d ]

  let rec arg_type : type t r. (t, r) atype -> Types.param_type = function
    | Arg (hd, tl) -> (None, elt_type hd) :: arg_type tl
    | NArg (name, hd, tl) -> (Some name, elt_type hd) :: arg_type tl
    | Res -> []

  let extern_type (Func (arg, res)) : Types.func_type =
    (arg_type arg, res_type res)

  type t =
    | WASM of func_id * Simplify_bis.func
    | Extern of extern_func

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      Fid !r

  let wasm func : t = WASM (fresh (), func)

  let extern f = Extern f

  let type_ = function
    | WASM (_, func) -> func.type_f
    | Extern (Extern_func (t, _f)) -> extern_type t
end

type externref = E : 'a externref_ty * 'a -> externref

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of ref_value

and ref_value =
  | Externref of externref option
  | Funcref of func option

and func = Func.t

type extern_func = Func.extern_func

let pp_ref fmt = function
  | Externref _ -> Format.fprintf fmt "externref"
  | Funcref _ -> Format.fprintf fmt "funcref"

let pp fmt = function
  | I32 i -> Format.fprintf fmt "i32.const %ld" i
  | I64 i -> Format.fprintf fmt "i64.const %Ld" i
  | F32 f -> Format.fprintf fmt "f32.const %a" Pp.Simplified.f32 f
  | F64 f -> Format.fprintf fmt "f64.const %a" Pp.Simplified.f64 f
  | Ref r -> pp_ref fmt r

let ref_null' (type_ : Types.ref_type) =
  match type_ with Func_ref -> Funcref None | Extern_ref -> Externref None

let ref_null (type_ : Types.ref_type) = Ref (ref_null' type_)

let ref_func (f : func) : t = Ref (Funcref (Some f))

let is_ref_null v =
  match v with Funcref None | Externref None -> true | _ -> false
