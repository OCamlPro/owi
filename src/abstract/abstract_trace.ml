(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type kind =
  | Block_start
  | Block_end
  | Step
  | Join
  | Widen

type state_trace =
  { stack : string list
  ; locals : string list
  ; call_stack : int list
  ; globals : string list
  }

type jump_target =
  { label : string
  ; states : state_trace list
  }

type named_state =
  { name : string
  ; state : state_trace option
  }

type event =
  { id : int
  ; instr_id : int
  ; instr : string
  ; context : string option
  ; state_trace : state_trace option
  ; kind : kind
  ; jts : jump_target list option
  ; inputs : named_state list option
  ; converged : bool option
  ; warnings : string list
  }

let enabled = ref false

let events = ref []

let pending_warnings = ref []

let next_id = ref 0

let enable () = enabled := true

let is_enabled () = !enabled

let reset () =
  events := [];
  pending_warnings := [];
  next_id := 0

let string_of_stack ctx stack =
  List.rev_map (fun v -> Fmt.str "%a" (Abstract_value.pp_with_ctx ctx) v) stack

let string_of_locals ctx locals =
  Abstract_locals.to_list locals
  |> List.map (fun (idx, v) ->
    Fmt.str "%i: %a" idx (Abstract_value.pp_with_ctx ctx) v )

let string_of_globals _ctx _globals = []

let trace_of_state (state : Abstract_state.t) =
  { stack = string_of_stack state.ctx state.stack
  ; locals = string_of_locals state.ctx state.locals
  ; call_stack = state.call_stack
  ; globals = string_of_globals state.ctx state
  }

let jump_targets_of_jt (jt : Abstract_jump_map.t) =
  Abstract_jump_map.to_list jt
  |> List.map (fun (k, v) ->
    let label = Fmt.str "%a" Abstract_jump_map.Key.pp k in
    let states =
      List.map
        (fun (istate : Abstract_interpreter_state.t) ->
          trace_of_state istate.abs_state )
        v
    in
    { label; states } )

let string_of_kind = function
  | Block_start -> "block_start"
  | Block_end -> "block_end"
  | Step -> "step"
  | Join -> "join"
  | Widen -> "widen"

let record_step ~kind ~(instr : Binary.instr Annotated.t)
  ~(inputs : (string * Abstract_state.t option) list option)
  ~(converged : bool option) ~(state : Abstract_state.t option) =
  if not !enabled then ()
  else
    let id = !next_id in
    incr next_id;
    let instr_id = instr.uuid in
    let instr = Fmt.str "%a" (Binary.pp_instr ~short:true) instr.raw in
    let state_trace = Option.map trace_of_state state in
    let context =
      Option.map
        (fun (state : Abstract_state.t) ->
          Fmt.str "%a" Abstract_domain.context_pretty state.ctx )
        state
    in
    let inputs =
      Option.map
        (List.map (fun (name, state) ->
           { name; state = Option.map trace_of_state state } ) )
        inputs
    in
    let warnings, still_pending =
      List.partition (fun (id, _) -> id = instr_id) !pending_warnings
    in
    pending_warnings := still_pending;
    let ev =
      { id
      ; instr_id
      ; instr
      ; state_trace
      ; kind
      ; jts = None
      ; context
      ; inputs
      ; converged
      ; warnings = List.map snd warnings
      }
    in
    events := ev :: !events

let record_jt ~(jt : Abstract_jump_map.t) =
  if not !enabled then ()
  else
    match !events with
    | [] -> ()
    | ev :: rest ->
      let jts = Some (jump_targets_of_jt jt) in
      events := { ev with jts } :: rest

let record_warning ~instr_id ~message =
  if not !enabled then ()
  else pending_warnings := (instr_id, message) :: !pending_warnings

let json_of_string_list l : Yojson.Safe.t =
  `List (List.map (fun s -> `String s) l)

let json_of_int_list l : Yojson.Safe.t =
  `List (List.rev_map (fun i -> `Int i) l)

let json_of_state_trace (st : state_trace) : Yojson.Safe.t =
  `Assoc
    [ ("stack", json_of_string_list st.stack)
    ; ("locals", json_of_string_list st.locals)
    ; ("call_stack", json_of_int_list st.call_stack)
    ; ("globals", json_of_string_list st.globals)
    ]

let json_of_jump_target (jt : jump_target) : Yojson.Safe.t =
  `Assoc
    [ ("label", `String jt.label)
    ; ("states", `List (List.map json_of_state_trace jt.states))
    ]

let json_of_named_state (ns : named_state) : Yojson.Safe.t =
  `Assoc
    [ ("name", `String ns.name)
    ; ( "state"
      , match ns.state with
        | None -> `Null
        | Some state -> json_of_state_trace state )
    ]

let json_of_event (ev : event) : Yojson.Safe.t =
  let state_fields =
    match ev.state_trace with
    | None ->
      [ ("stack", `Null)
      ; ("locals", `Null)
      ; ("call_stack", `Null)
      ; ("globals", `Null)
      ]
    | Some st ->
      [ ("stack", json_of_string_list st.stack)
      ; ("locals", json_of_string_list st.locals)
      ; ("call_stack", json_of_int_list st.call_stack)
      ; ("globals", json_of_string_list st.globals)
      ]
  in
  `Assoc
    ( [ ("id", `Int ev.id)
      ; ("instr_id", `Int ev.instr_id)
      ; ("instr", `String ev.instr)
      ; ("kind", `String (string_of_kind ev.kind))
      ; ( "context"
        , match ev.context with
          | None -> `Null
          | Some context -> `String context )
      ; ( "jts"
        , match ev.jts with
          | None -> `Null
          | Some jts -> `List (List.map json_of_jump_target jts) )
      ; ( "inputs"
        , match ev.inputs with
          | None -> `Null
          | Some inputs -> `List (List.map json_of_named_state inputs) )
      ; ( "converged"
        , match ev.converged with
          | None -> `Null
          | Some converged -> `Bool converged )
      ; ("warnings", json_of_string_list (List.rev ev.warnings))
      ]
    @ state_fields )

let write_json path =
  let sorted = List.rev !events in
  let json = `List (List.map json_of_event sorted) in
  let contents = Fmt.str "%s\n" (Yojson.Safe.pretty_to_string json) in
  match Bos.OS.File.write (Fpath.v path) contents with
  | Ok () -> ()
  | Error (`Msg msg) -> Log.err (fun m -> m "Failed to write trace: %s" msg)
