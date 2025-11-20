module Module = struct
  (** extern modules *)
  type 'extern_func t =
    { functions : (string * 'extern_func) list
    ; func_type : 'extern_func -> Binary.func_type
    }
end
