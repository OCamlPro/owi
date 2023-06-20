(** runtime stack module *)

module Make (V : Stack_intf.Value) :
  Stack_intf.S
    with type 'a value := 'a V.t
     and type vbool := V.vbool
     and type int32 := V.int32
     and type int64 := V.int64
     and type float32 := V.float32
     and type float64 := V.float64
     and type 'a ref_value := 'a V.ref_value
