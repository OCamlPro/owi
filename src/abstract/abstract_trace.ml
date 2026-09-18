(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type kind =
  | Block_start
  | Block_end
  | Step
  | Join
  | Widen

type wasm_event =
  { id : int
  ; instr_id : int
  ; instr : string
  ; context : string option
      (* Context is mutable so we pretty print the current state directly *)
  ; state : Abstract_interpreter_state.t option
  ; kind : kind
  ; mutable jm : Abstract_jump_map.t
  ; inputs : (string * Abstract_interpreter_state.t option) list
  ; converged : bool option
  ; warnings : string list
  }

type test_res =
  | Ok
  | Fail of
      { expected : string
      ; got : string
      }

type wast_event =
  { id : int
  ; kind : kind
  ; cmd : string
  ; test_res : test_res
  }

type event =
  | Wast of wast_event
  | Wasm of wasm_event

let string_of_kind = function
  | Block_start -> "block_start"
  | Block_end -> "block_end"
  | Step -> "step"
  | Join -> "join"
  | Widen -> "widen"

let enabled = ref false

let events = ref []

let pending_warnings = ref []

let pending_test_res = ref None

let next_id = ref 0

let enable () = enabled := true

let is_enabled () = !enabled

let record_wasm_step
  ?(inputs : (string * Abstract_interpreter_state.t option) list = [])
  ?(converged : bool option = None) (kind : kind)
  (state : Abstract_interpreter_state.t option)
  (instr : Binary.instr Annotated.t) =
  if not !enabled then ()
  else
    let id = !next_id in
    incr next_id;
    let instr_id = instr.uuid in
    let instr = Fmt.to_to_string (Binary.pp_instr ~short:true) instr.raw in
    let context =
      Option.map
        (fun (state : Abstract_interpreter_state.t) ->
          Fmt.to_to_string Abstract_domain.context_pretty state.abs_state.ctx )
        state
    in
    let warnings, still_pending =
      List.partition (fun (id, _) -> id = instr_id) !pending_warnings
    in
    pending_warnings := still_pending;
    let ev =
      { id
      ; instr_id
      ; instr
      ; state
      ; kind
        (* We record the jts separately as we want to trace the jump targets
        after the current jump targets has been merged with existing jump targets *)
      ; jm = Abstract_jump_map.empty
      ; context
      ; inputs
      ; converged
      ; warnings = List.map snd warnings
      }
    in
    events := Wasm ev :: !events

let record_wasm_jt ~(jm : Abstract_jump_map.t) =
  if not !enabled then ()
  else match !events with Wasm ev :: _ -> ev.jm <- jm | _ -> assert false

let record_wasm_warning ~instr_id ~message =
  if not !enabled then ()
  else pending_warnings := (instr_id, message) :: !pending_warnings

let record_wast_cmd kind cmd =
  if not !enabled then ()
  else
    let id = !next_id in
    incr next_id;
    let cmd =
      match kind with Block_end -> "" | _ -> Fmt.to_to_string Wast.pp_cmd cmd
    in
    let test_res =
      match !pending_test_res with Some test_res -> test_res | None -> Ok
    in
    let ev = { id; kind; cmd; test_res } in
    events := Wast ev :: !events;
    pending_test_res := None

let record_wast_test_res test_res =
  if not !enabled then () else pending_test_res := Some test_res

let json_of_string_list l : Yojson.Safe.t =
  `List (List.map (fun s -> `String s) l)

let json_of_int_list l : Yojson.Safe.t =
  `List (List.rev_map (fun i -> `Int i) l)

let assoc_of_state (state : Abstract_interpreter_state.t) :
  (string * Yojson.Safe.t) list =
  let stack =
    List.rev_map
      (fun s ->
        `String
          (Fmt.to_to_string (Abstract_value.pp_with_ctx state.abs_state.ctx) s) )
      state.abs_state.stack
  in
  let stack = `List stack in
  let locals =
    List.map
      (fun (idx, value) ->
        let v =
          Fmt.to_to_string
            (Abstract_value.pp_with_ctx state.abs_state.ctx)
            value
        in
        (string_of_int idx, `String v) )
      (Abstract_locals.to_list state.abs_state.locals)
  in
  let locals = `Assoc locals in
  let globals =
    `String
      (Fmt.to_to_string
         (Env.Abstract.pp_globals
            (Abstract_value.pp_with_ctx state.abs_state.ctx) )
         state.env )
  in
  [ ("stack", stack)
  ; ("locals", locals)
  ; ("call_stack", json_of_int_list state.abs_state.call_stack)
  ; ("globals", globals)
  ]

let json_of_jump_map (jts : Abstract_jump_map.t) : Yojson.Safe.t =
  let list =
    Abstract_jump_map.to_list jts
    |> List.map (fun (k, states) ->
      ( Fmt.to_to_string Abstract_jump_map.Key.pp k
      , `List (List.map (fun s -> `Assoc (assoc_of_state s)) states) ) )
  in
  `Assoc list

let json_of_named_state
  ((name, state) : string * Abstract_interpreter_state.t option) =
  `Assoc
    [ ("name", `String name)
    ; ( "state"
      , match state with
        | None -> `Null
        | Some state -> `Assoc (assoc_of_state state) )
    ]

let json_of_test_res (res : test_res) : Yojson.Safe.t =
  match res with
  | Ok -> `String "ok"
  | Fail { expected; got } ->
    `Assoc [ ("expected", `String expected); ("got", `String got) ]

let json_of_event (ev : event) : Yojson.Safe.t =
  match ev with
  | Wasm ev -> begin
    let state_fields =
      match ev.state with None -> [] | Some st -> assoc_of_state st
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
        ; ("jts", json_of_jump_map ev.jm)
        ; ("inputs", `List (List.map json_of_named_state ev.inputs))
        ; ( "converged"
          , match ev.converged with
            | None -> `Null
            | Some converged -> `Bool converged )
        ; ("warnings", json_of_string_list (List.rev ev.warnings))
        ]
      @ state_fields )
    end
  | Wast { id; kind; cmd; test_res } ->
    `Assoc
      [ ("id", `Int id)
      ; ("kind", `String (string_of_kind kind))
      ; ("cmd", `String cmd)
      ; ("test_res", json_of_test_res test_res)
      ]

let write_json path =
  let reved = List.rev !events in
  let json : Yojson.Safe.t =
    `Assoc
      [ ( "metadata"
        , `Assoc
            [ ("version", `Float 1.)
            ; ("mode", `String "abstract")
            ; ("script", `Bool false)
            ] )
      ; ("events", `List (List.map json_of_event reved))
      ]
  in
  let contents = Fmt.str "%s\n" (Yojson.Safe.pretty_to_string json) in
  match Bos.OS.File.write (Fpath.v path) contents with
  | Ok () -> ()
  | Error (`Msg msg) -> Log.err (fun m -> m "Failed to write trace: %s" msg)
