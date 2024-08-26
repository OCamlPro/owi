module Backend = struct
  open Smtml
  include Symbolic_memory_base

  type address = Int32.t

  type t =
    { data : (address, Symbolic_value.int32) Hashtbl.t
    ; parent : t option
    ; chunks : (address, Symbolic_value.int32) Hashtbl.t
    }

  let make () =
    { data = Hashtbl.create 16; parent = None; chunks = Hashtbl.create 16 }

  let clone m =
    (* TODO: Make chunk copying lazy *)
    { data = Hashtbl.create 16
    ; parent = Some m
    ; chunks = Hashtbl.copy m.chunks
    }

  let address a =
    let open Symbolic_choice_without_memory in
    match Expr.view a with
    | Val (Num (I32 i)) -> return i
    | Ptr { base; offset } ->
      select_i32 Symbolic_value.(I32.add (const_i32 base) offset)
    | _ -> select_i32 a

  let address_i32 a = a

  let rec load_byte { parent; data; _ } a =
    try Hashtbl.find data a
    with Not_found -> (
      match parent with
      | None -> Expr.value (Num (I8 0))
      | Some parent -> load_byte parent a )

  let loadn m a n =
    let rec loop addr i acc =
      if i = n then acc
      else
        let addr' = Int32.(add addr (of_int i)) in
        let byte = load_byte m addr' in
        loop addr (i + 1) (concat i ~msb:byte ~lsb:acc)
    in
    let v0 = load_byte m a in
    loop a 1 v0

  let storen m a v n =
    for i = 0 to n - 1 do
      let a' = Int32.add a (Int32.of_int i) in
      let v' = extract v i in
      Hashtbl.replace m.data a' v'
    done

  let validate_address m a =
    let open Symbolic_choice_without_memory in
    match Smtml.Expr.view a with
    | Val (Num (I32 _)) -> return (Ok a) (* An i32 is a valid address *)
    | Ptr { base; offset } -> (
      let open Symbolic_value in
      (* A pointer is valid if it's within bounds. *)
      match Hashtbl.find m.chunks base with
      | exception Not_found -> return (Error Trap.Memory_leak_use_after_free)
      | size ->
        let+ is_out_of_bounds = select (I32.ge_u offset size) in
        if is_out_of_bounds then Error Trap.Memory_heap_buffer_overflow
        else Ok a )
    | _ ->
      (* A symbolic expression should be a valid address *)
      return (Ok a)

  let ptr v =
    let open Symbolic_choice_without_memory in
    match Expr.view v with
    | Ptr { base; _ } -> return base
    | _ ->
      Log.debug2 {|free: cannot fetch pointer base of "%a"|} Expr.pp v;
      let* () = add_pc @@ Expr.value False in
      assert false

  let free m p =
    let open Symbolic_choice_without_memory in
    let+ base = ptr p in
    if not @@ Hashtbl.mem m.chunks base then
      Fmt.failwith "Memory leak double free";
    Hashtbl.remove m.chunks base

  let realloc m ~ptr ~size =
    let open Symbolic_choice_without_memory in
    let+ base = address ptr in
    Hashtbl.replace m.chunks base size;
    Expr.ptr base (Symbolic_value.const_i32 0l)

  let rec pp fmt m =
    let pp_parent =
      Fmt.option ~none:(fun fmt () -> Fmt.string fmt "None (root mem)") pp
    in
    let pp_v fmt (a, b) = Fmt.pf fmt "0x%08lx %a" a Expr.pp b in
    Fmt.pf fmt "@[<v>parent:@;@[<v 1> %a@]@;data:@;@[<v 1> %a@]@]" pp_parent
      m.parent (Fmt.hashtbl pp_v) m.data
end

include Symbolic_memory_make.Make (Backend)
