open Types
open Types.Simplified

module type P = sig
  type memory

  type 'env table

  (** runtime memory *)
  module Memory : sig
    type t = memory

    val get_data : t -> bytes

    val get_limit_max : t -> int option

    val get_limits : t -> limits

    val update_memory : t -> bytes -> unit

    val init : ?label:string -> limits -> t
  end

  (** runtime table *)
  module Table : sig
    type 'env t = 'env table

    type 'env table_data

    val init : ?label:string -> table_type -> 'env t

    val update : 'a t -> 'a table_data -> unit

    val limits : 'a t -> limits

    val typ : 'a t -> ref_type
  end
end
