module Func = struct
  module type T = sig
    type i32

    type i64

    type f32

    type f64

    type v128

    type 'a m

    type memory

    type _ telt = private
      | I32 : i32 telt
      | I64 : i64 telt
      | F32 : f32 telt
      | F64 : f64 telt
      | V128 : v128 telt
      | Externref : 'a Type.Id.t -> 'a telt

    type (_, _) atype = private
      | Mem : int * ('b, 'r) atype -> (memory -> 'b, 'r) atype
      | UArg : ('b, 'r) atype -> (unit -> 'b, 'r) atype
      | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
      | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
      | Res : ('r, 'r) atype

    type _ rtype = private
      | R0 : unit rtype
      | R1 : 'a telt -> 'a rtype
      | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
      | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
      | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

    type _ func_type = private
      | Func : ('f, 'r m) atype * 'r rtype -> 'f func_type

    type extern_func = Extern_func : 'a func_type * 'a -> extern_func

    val extern_type : extern_func -> Text.func_type

    module Syntax : sig
      type l

      type lr

      type elt

      type mem

      type (_, _, _) t

      val i32 : (lr, elt, i32) t

      val i64 : (lr, elt, i64) t

      val f32 : (lr, elt, f32) t

      val f64 : (lr, elt, f64) t

      val v128 : (lr, elt, v128) t

      val externref : 'a Type.Id.t -> (lr, elt, 'a) t

      val unit : (lr, unit, unit) t

      val memory : int -> (l, mem, memory) t

      val label : string -> (lr, elt, 'a) t -> (l, string * elt, 'a) t

      val ( ^-> ) : ('r, 'k, 'a) t -> 'b func_type -> ('a -> 'b) func_type

      val ( ^->. ) : ('r, 'k, 'a) t -> (lr, 'kk, 'b) t -> ('a -> 'b m) func_type

      val ( ^->.. ) :
           ('ll, 'k, 'a) t
        -> (lr, elt, 'b1) t * (lr, elt, 'b2) t
        -> ('a -> ('b1 * 'b2) m) func_type

      val ( ^->... ) :
           ('ll, 'k, 'a) t
        -> (lr, elt, 'b1) t * (lr, elt, 'b2) t * (lr, elt, 'b3) t
        -> ('a -> ('b1 * 'b2 * 'b3) m) func_type

      val ( ^->.... ) :
           ('ll, 'k, 'a) t
        -> (lr, elt, 'b1) t
           * (lr, elt, 'b2) t
           * (lr, elt, 'b3) t
           * (lr, elt, 'b4) t
        -> ('a -> ('b1 * 'b2 * 'b3 * 'b4) m) func_type
    end
  end

  module Make (Value : sig
    type i32

    type i64

    type f32

    type f64

    type v128
  end) (M : sig
    (** The monad type *)
    type 'a t
  end) (Memory : sig
    (** The memory type *)
    type t
  end) : sig
    val fresh : unit -> int

    include
      T
        with type i32 := Value.i32
         and type i64 := Value.i64
         and type f32 := Value.f32
         and type f64 := Value.f64
         and type v128 := Value.v128
         and type 'a m := 'a M.t
         and type memory := Memory.t
  end = struct
    type 'a m = 'a M.t

    type memory = Memory.t

    type _ telt =
      | I32 : Value.i32 telt
      | I64 : Value.i64 telt
      | F32 : Value.f32 telt
      | F64 : Value.f64 telt
      | V128 : Value.v128 telt
      | Externref : 'a Type.Id.t -> 'a telt

    type _ rtype =
      | R0 : unit rtype
      | R1 : 'a telt -> 'a rtype
      | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
      | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
      | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

    type (_, _) atype =
      | Mem : int * ('b, 'r) atype -> (memory -> 'b, 'r) atype
      | UArg : ('b, 'r) atype -> (unit -> 'b, 'r) atype
      | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
      | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
      | Res : ('r, 'r) atype

    type _ func_type = Func : ('f, 'r m) atype * 'r rtype -> 'f func_type

    type extern_func = Extern_func : 'a func_type * 'a -> extern_func

    let elt_type (type t) (e : t telt) : Text.val_type =
      match e with
      | I32 -> Num_type I32
      | I64 -> Num_type I64
      | F32 -> Num_type F32
      | F64 -> Num_type F64
      | V128 -> Num_type V128
      | Externref _ -> Ref_type (Null, Extern_ht)

    let res_type (type t) (r : t rtype) : Text.result_type =
      match r with
      | R0 -> []
      | R1 a -> [ elt_type a ]
      | R2 (a, b) -> [ elt_type a; elt_type b ]
      | R3 (a, b, c) -> [ elt_type a; elt_type b; elt_type c ]
      | R4 (a, b, c, d) -> [ elt_type a; elt_type b; elt_type c; elt_type d ]

    let rec arg_type : type t r. (t, r) atype -> Text.param_type = function
      | Mem (_, tl) -> arg_type tl
      | UArg tl -> arg_type tl
      | Arg (hd, tl) -> (None, elt_type hd) :: arg_type tl
      | NArg (name, hd, tl) -> (Some name, elt_type hd) :: arg_type tl
      | Res -> []

    (* TODO: we could move this out, as it does not really depend on the functor's parameters *)
    let extern_type (Extern_func (Func (arg, res), _)) : Text.func_type =
      (arg_type arg, res_type res)

    let fresh =
      let r = ref ~-1 in
      fun () ->
        incr r;
        !r

    module Syntax = struct
      type l

      type lr

      type elt

      type mem

      type (_, _, _) t =
        | Unit : (lr, unit, unit) t
        | Memory : int -> (l, mem, memory) t
        | Elt : 'a telt -> (lr, elt, 'a) t
        | Elt_labeled : string * 'a telt -> (l, string * elt, 'a) t

      let return r = Func (Res, r)

      let r0 = R0 |> return

      let r1 (Elt a) = R1 a |> return

      let r2 (Elt a) (Elt b) = R2 (a, b) |> return

      let r3 (Elt a) (Elt b) (Elt c) = R3 (a, b, c) |> return

      let r4 (Elt a) (Elt b) (Elt c) (Elt d) = R4 (a, b, c, d) |> return

      let i32 = Elt I32

      let i64 = Elt I64

      let f32 = Elt F32

      let f64 = Elt F64

      let v128 = Elt V128

      let externref id = Elt (Externref id)

      let unit = Unit

      let memory id = Memory id

      let label s (Elt v) = Elt_labeled (s, v)

      let ( ^-> ) : type lr k a b.
        (lr, k, a) t -> b func_type -> (a -> b) func_type =
       fun a (Func (b, r)) ->
        match a with
        | Elt a -> Func (Arg (a, b), r)
        | Elt_labeled (label, a) -> Func (NArg (label, a, b), r)
        | Unit -> Func (UArg b, r)
        | Memory id -> Func (Mem (id, b), r)

      let ( ^->. ) : type ll k kk a b.
        (ll, k, a) t -> (lr, kk, b) t -> (a -> b m) func_type =
       fun a b -> match b with Elt _ -> a ^-> r1 b | Unit -> a ^-> r0

      let ( ^->.. ) : type ll k a b1 b2.
           (ll, k, a) t
        -> (lr, elt, b1) t * (lr, elt, b2) t
        -> (a -> (b1 * b2) m) func_type =
       fun a (b1, b2) -> a ^-> r2 b1 b2

      let ( ^->... ) : type ll k a b1 b2 b3.
           (ll, k, a) t
        -> (lr, elt, b1) t * (lr, elt, b2) t * (lr, elt, b3) t
        -> (a -> (b1 * b2 * b3) m) func_type =
       fun a (b1, b2, b3) -> a ^-> r3 b1 b2 b3

      let ( ^->.... ) : type ll k a b1 b2 b3 b4.
           (ll, k, a) t
        -> (lr, elt, b1) t * (lr, elt, b2) t * (lr, elt, b3) t * (lr, elt, b4) t
        -> (a -> (b1 * b2 * b3 * b4) m) func_type =
       fun a (b1, b2, b3, b4) -> a ^-> r4 b1 b2 b3 b4
    end
  end
end

module Module = struct
  (** extern modules *)
  type 'extern_func t =
    { functions : (string * 'extern_func) list
    ; func_type : 'extern_func -> Text.func_type
    }
end
