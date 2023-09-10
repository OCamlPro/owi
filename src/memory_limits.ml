open Types

let page_size = 65_536

module Make (M : Interpret_functor_intf.Memory_data) = struct
  type t =
    { id : int
    ; label : string option
    ; limits : limits
    ; mutable data : M.t
    }

  let fresh =
    let r = ref (-1) in
    fun () ->
      incr r;
      !r

  let update_memory mem data = mem.data <- data

  let get_limit_max { limits; _ } = limits.max

  let get_limits { limits; _ } = limits

  let load_8_s mem addr = M.load_8_s mem.data addr

  let load_8_u mem addr = M.load_8_u mem.data addr

  let load_16_s mem addr = M.load_16_s mem.data addr

  let load_16_u mem addr = M.load_16_u mem.data addr

  let load_32 mem addr = M.load_32 mem.data addr

  let load_64 mem addr = M.load_64 mem.data addr

  let store_8 mem ~addr value = M.store_8 mem.data ~addr value

  let store_16 mem ~addr value = M.store_16 mem.data ~addr value

  let store_32 mem ~addr value = M.store_32 mem.data ~addr value

  let store_64 mem ~addr value = M.store_64 mem.data ~addr value

  let create label limits size =
    { id = fresh (); label; limits; data = M.create size }

  let grow t size = M.grow t.data size
  (* let limits = *)
  (*   { mem.limits with min = max mem.limits.min (size / page_size) } *)
  (* in *)
  (* mem.limits <- limits; *)
  (* t.data <- data *)
end
