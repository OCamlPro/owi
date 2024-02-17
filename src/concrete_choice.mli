(* TODO: type 'a t should be abstract, run will be needed for this *)
include Choice_intf.Base with type 'a t = 'a and module V := V

val run : 'a t -> 'a
