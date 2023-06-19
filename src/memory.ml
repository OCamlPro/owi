open Types
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let page_size = 65_536

type t =
  { id : int
  ; label : string option
  ; mutable limits : limits
  ; mutable data : bytes
  }

let fresh =
  let r = ref (-1) in
  fun () ->
    incr r;
    !r

let init ?label (typ : limits) : t =
  let data = Bytes.make (page_size * typ.min) '\x00' in
  { id = fresh (); label; limits = typ; data }

let update_memory mem data =
  let limits =
    { mem.limits with min = max mem.limits.min (Bytes.length data / page_size) }
  in
  mem.limits <- limits;
  mem.data <- data

let get_data { data; _ } = data

let get_limit_max { limits; _ } = limits.max

let get_limits { limits; _ } = limits
