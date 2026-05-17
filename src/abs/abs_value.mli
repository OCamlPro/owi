(* recup le context depuis le monade *)

module Boolean : sig
  type t := Abstract_value0.boolean

  val of_i32 : Abstract_value0.i32 -> t

  val to_i32 : t -> Abstract_value0.i32

  val _true : t

  val _false : t

  val not : Abstract_value0.boolean -> Abstract_value0.boolean
end

module I32 : sig
  type t := Abstract_value0.i32

  val zero : t

  val of_concrete : Concrete_i32.t -> t

  val eqz : t -> bool

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : Text.sx -> t -> t -> t

  val to_int : t -> int

  val logand : t -> t -> t

  val logor : t -> t -> t

  (* val lt : ?state:State.t -> t -> t -> boolean *)
  (**)
  (* val le : ?state:State.t -> t -> t -> boolean *)
  (**)
  (* val gt : ?state:State.t -> t -> t -> boolean *)
  (**)
  (* val ge : ?state:State.t -> t -> t -> boolean *)
end

module I64 : sig
  type t := Abstract_value0.i64

  val zero : t

  val of_concrete : Concrete_i64.t -> t

  val eqz : t -> bool

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : Text.sx -> t -> t -> t

  val logand : t -> t -> t

  val logor : t -> t -> t
end

val pp : Abstract_value0.t Fmt.t
