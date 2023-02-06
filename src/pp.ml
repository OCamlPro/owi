(** Module to pretty print the various types such as text module or simplified
    module. *)

(*

module Make_Expr (Arg : Arg) = struct
  include Shared
end

*)

(*
module Input_Expr = Make_Expr (Symbolic_indice)

module Global = struct
  open Input_Expr


  let pos out { Ppxlib.pos_lnum; pos_cnum; pos_bol; _ } =
    Format.fprintf out "line %d:%d" pos_lnum (pos_cnum - pos_bol)
end
*)
