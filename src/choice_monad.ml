open Choice_monad_intf

type vbool = Sym_value.S.vbool

type vint32 = Sym_value.S.int32

exception Assertion of assertion * Thread.t

let check (sym_bool : vbool) (state : Thread.t) : bool =
  let solver = Thread.solver state in
  let pc = Thread.pc state in
  let no = Sym_value.S.Bool.not sym_bool in
  let no = Encoding.Expression.simplify no in
  match no with
  | Val (Bool no) -> not no
  | _ ->
    let check = no :: pc in
    Format.printf "CHECK:@.";
    List.iter (fun c -> print_endline (Encoding.Expression.to_string c)) check;
    let r = Thread.Solver.check solver check in
    let msg = if r then "KO" else "OK" in
    Format.printf "/CHECK %s@." msg;
    not r

let eval_choice (sym_bool : vbool) (state : Thread.t) : (bool * Thread.t) list =
  let solver = Thread.solver state in
  let pc = Thread.pc state in
  let memories = Thread.memories state in
  let tables = Thread.tables state in
  let globals = Thread.globals state in
  let sym_bool = Encoding.Expression.simplify sym_bool in
  match sym_bool with
  | Val (Bool b) -> [ (b, state) ]
  | Val (Num (I32 _)) -> assert false
  | _ -> (
    let no = Sym_value.S.Bool.not sym_bool in
    let sat_true = Thread.Solver.check solver (sym_bool :: pc) in
    let sat_false = Thread.Solver.check solver (no :: pc) in
    match (sat_true, sat_false) with
    | false, false -> []
    | true, false -> [ (true, state) ]
    | false, true -> [ (false, state) ]
    | true, true ->
      Format.printf "CHOICE: %s@." (Encoding.Expression.to_string sym_bool);
      let state1 : Thread.t =
        { solver
        ; pc = sym_bool :: pc
        ; memories = Sym_memory.clone memories
        ; tables = Sym_table.clone tables
        ; globals = Sym_global.clone globals
        }
      in
      let state2 : Thread.t =
        { solver
        ; pc = no :: pc
        ; memories = Sym_memory.clone memories
        ; tables = Sym_table.clone tables
        ; globals = Sym_global.clone globals
        }
      in
      [ (true, state1); (false, state2) ] )

let fix_symbol (e : Encoding.Expression.t) pc =
  let open Encoding in
  match e with
  | Symbol sym -> (pc, sym)
  | _ ->
    let sym = Symbol.mk_symbol `I32Type "choice_i32" in
    let assign = Expression.Relop (I32 Eq, Symbol sym, e) in
    (assign :: pc, sym)

let clone_if_needed ~orig_pc (cases : (int32 * Thread.t) list) :
  (int32 * Thread.t) list =
  match cases with
  | [] -> []
  | [ (i, state) ] -> [ (i, { state with pc = orig_pc }) ]
  | _ :: _ :: _ ->
    List.map
      (fun (i, state) ->
        let solver = Thread.solver state in
        let pc = Thread.pc state in
        let memories = Thread.memories state in
        let tables = Thread.tables state in
        let globals = Thread.globals state in
        let state : Thread.t =
          { solver
          ; pc
          ; memories = Sym_memory.clone memories
          ; tables = Sym_table.clone tables
          ; globals = Sym_global.clone globals
          }
        in
        (i, state) )
      cases

let not_value sym value =
  Encoding.Expression.Relop (I32 Ne, Symbol sym, Val (Num (I32 value)))

let eval_choice_i32 (sym_int : vint32) (state : Thread.t) :
  (int32 * Thread.t) list =
  let module Solver = Thread.Solver in
  let solver = Thread.solver state in
  let pc = Thread.pc state in
  let sym_int = Encoding.Expression.simplify sym_int in
  let orig_pc = pc in
  let pc, sym = fix_symbol sym_int pc in
  match sym_int with
  | Val (Num (I32 i)) -> [ (i, state) ]
  | _ ->
    let rec find_values values =
      let additionnal = List.map (not_value sym) values in
      if not (Solver.check solver (additionnal @ pc)) then []
      else begin
        let model = Solver.model ~symbols:[ sym ] solver in
        match model with
        | None -> assert false (* ? *)
        | Some model -> (
          let desc = Encoding.Model.to_string model in
          Format.printf "Model:@.%s@." desc;
          let v = Encoding.Model.evaluate model sym in
          match v with
          | None -> assert false (* ? *)
          | Some (Num (I32 i) as v) -> begin
            let cond = Encoding.Expression.Relop (I32 Eq, Symbol sym, Val v) in
            let case = (i, { state with pc = cond :: pc }) in
            case :: find_values (i :: values)
          end
          | Some _ -> assert false )
      end
    in
    let cases = find_values [] in
    clone_if_needed ~orig_pc cases

module List = struct
  type vbool = Sym_value.S.vbool

  type thread = Thread.t

  type 'a t = thread -> ('a * thread) list

  let return (v : 'a) : 'a t = fun t -> [ (v, t) ]

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let lst = v t in
    match lst with
    | [] -> []
    | [ (r, t) ] -> (f r) t
    | _ -> List.concat_map (fun (r, t) -> (f r) t) lst

  let select (sym_bool : vbool) : bool t = eval_choice sym_bool

  let select_i32 (i : Sym_value.S.int32) : int32 t = eval_choice_i32 i

  let trap : Trap.t -> 'a t = function
    | Unreachable -> fun _ -> []
    | _ -> assert false

  (* raise (Types.Trap "out of bounds memory access") *)

  let with_thread (f : thread -> 'b) : 'b t = fun t -> [ (f t, t) ]

  let add_pc (c : Sym_value.S.vbool) : unit t =
   fun t -> [ ((), { t with pc = c :: t.pc }) ]

  let assertion c t =
    if check c t then [ ((), t) ]
    else raise (Assertion (Encoding.Expression.to_string c, t))

  let run (v : 'a t) (thread : thread) = List.to_seq (v thread)
end

module Seq = struct
  module List = Stdlib.List

  type vbool = Sym_value.S.vbool

  type thread = Thread.t

  type 'a t = thread -> ('a * thread) Seq.t

  let return (v : 'a) : 'a t = fun t -> Seq.return (v, t)

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let seq = v t in
    Seq.flat_map (fun (e, t) -> f e t) seq

  let select (sym_bool : vbool) : bool t =
   fun state -> List.to_seq (eval_choice sym_bool state)

  let select_i32 (i : Sym_value.S.int32) : int32 t =
   fun state -> List.to_seq (eval_choice_i32 i state)

  let trap : Trap.t -> 'a t = function
    | Unreachable -> fun _ -> Seq.empty
    | _ -> assert false

  let assertion c t =
    if check c t then Seq.return ((), t)
    else raise (Assertion (Encoding.Expression.to_string c, t))

  (* raise (Types.Trap "out of bounds memory access") *)

  let with_thread (f : thread -> 'b) : 'b t = fun t -> Seq.return (f t, t)

  let add_pc (c : Sym_value.S.vbool) : unit t =
   fun t -> Seq.return ((), { t with pc = c :: t.pc })

  let run (v : 'a t) (thread : thread) = v thread
end

module Explicit = struct
  module List = Stdlib.List
  module Seq = Stdlib.Seq

  type vbool = Sym_value.S.vbool

  type thread = Thread.t

  type 'a st = St of (thread -> 'a * thread) [@@unboxed]

  type 'a t =
    | Empty : 'a t
    | Ret : 'a st -> 'a t
    | Retv : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Assert : vbool -> unit t
    | Choice : vbool -> bool t
    | Choice_i32 : vint32 -> int32 t
    | Trap : Trap.t -> 'a t

  let return (v : 'a) : 'a t = Retv v [@@inline]

  let bind : type a b. a t -> (a -> b t) -> b t =
   fun v f ->
    match v with
    | Empty -> Empty
    | Trap t -> Trap t
    | Retv v -> f v
    | Assert _ | Ret _ | Choice _ | Choice_i32 _ -> Bind (v, f)
    | Bind _ -> Bind (v, f)
   [@@inline]

  (* let rec bind : type a b. a t -> (a -> b t) -> b t =
   *  fun v f ->
   *   match v with
   *   | Empty -> Empty
   *   | Retv v -> f v
   *   | Ret _ | Choice _ -> Bind (v, f)
   *   | Bind (v, f1) -> Bind (v, fun x -> bind (f1 x) f)
   *  [@@inline] *)

  let select (cond : vbool) : bool t =
    match cond with Val (Bool b) -> Retv b | _ -> Choice cond
    [@@inline]

  let select_i32 (i : Sym_value.S.int32) : int32 t =
    match i with Val (Num (I32 v)) -> Retv v | _ -> Choice_i32 i

  let trap : Trap.t -> 'a t = fun t -> Trap t

  let with_thread (f : thread -> 'b) : 'b t = Ret (St (fun t -> (f t, t)))
    [@@inline]

  let add_pc (c : Sym_value.S.vbool) : unit t =
    Ret (St (fun t -> ((), { t with pc = c :: t.pc })))
    [@@inline]

  let assertion c : unit t = Assert c

  let rec run : type a. a t -> thread -> (a * thread) Seq.t =
   fun v t ->
    match v with
    | Empty -> Seq.empty
    | Trap _t -> Seq.empty (* TODO do something useful with the trap *)
    | Retv v -> Seq.return (v, t)
    | Ret (St f) -> Seq.return (f t)
    | Bind (v, f) -> Seq.flat_map (fun (v, t) -> run (f v) t) (run v t)
    | Assert c ->
      if check c t then Seq.return ((), t)
      else raise (Assertion (Encoding.Expression.to_string c, t))
    | Choice cond -> List.to_seq (eval_choice cond t)
    | Choice_i32 i -> List.to_seq (eval_choice_i32 i t)

  let rec run_and_trap : type a. a t -> thread -> (a eval * thread) Seq.t =
   fun v t ->
    match v with
    | Empty -> Seq.empty
    | Trap tr -> Seq.return (ETrap tr, t)
    | Retv v -> Seq.return (EVal v, t)
    | Ret (St f) ->
      let v, t = f t in
      Seq.return (EVal v, t)
    | Bind (v, f) ->
      Seq.flat_map
        (fun (v, t) ->
          match v with
          | EAssert f -> Seq.return (EAssert f, t)
          | ETrap tr -> Seq.return (ETrap tr, t)
          | EVal v -> run_and_trap (f v) t )
        (run_and_trap v t)
    | Assert c ->
      if check c t then Seq.return (EVal (), t)
      else Seq.return (EAssert (Encoding.Expression.to_string c), t)
    | Choice cond ->
      List.to_seq (List.map (fun (v, t) -> (EVal v, t)) (eval_choice cond t))
    | Choice_i32 i ->
      List.to_seq (List.map (fun (v, t) -> (EVal v, t)) (eval_choice_i32 i t))

  let rec run_up_to : type a. depth:int -> a t -> thread -> (a * thread) Seq.t =
   fun ~depth v t ->
    match v with
    | Empty -> Seq.empty
    | Trap _t -> Seq.empty (* TODO do something useful with the trap *)
    | Retv v -> Seq.return (v, t)
    | Ret (St f) -> Seq.return (f t)
    | Bind (v, f) -> begin
      match v with
      | Choice _ when depth <= 0 -> Seq.empty
      | _ ->
        Seq.flat_map
          (fun (v, t) -> run (f v) t)
          (run_up_to ~depth:(depth - 1) v t)
    end
    | Assert c ->
      if check c t then Seq.return ((), t)
      else raise (Assertion (Encoding.Expression.to_string c, t))
    | Choice cond -> List.to_seq (eval_choice cond t)
    | Choice_i32 i -> List.to_seq (eval_choice_i32 i t)
end

module type T =
  Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Sym_value.S

module type T_trap =
  Choice_monad_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Sym_value.S

let list = (module List : T)

let seq = (module Seq : T)

let explicit = (module Explicit : T)

let choices = [ list; seq; explicit ]
