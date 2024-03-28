(* binary format specification:
   https://webassembly.github.io/spec/core/binary/modules.html#binary-importsec *)

open Simplified
open Syntax
open Types
open Binary_basic

let consume_to_end x input = (x, Input.sub ~pos:0 ~len:0 input)

let magic_check bytes =
  let magic = Bytes.sub_string bytes 0 4 in
  match String.equal magic "\x00\x61\x73\x6d" with
  | true -> Ok ()
  | false -> assert false

let version_check bytes =
  let version = Bytes.sub_string bytes 4 4 in
  match String.equal version "\x01\x00\x00\x00" with
  | true -> Ok ()
  | false -> assert false

let section_parse input ~expected_id default section_content_parse =
  match Input.get0 input with
  | Some id when id = expected_id ->
    let input = Input.sub_suffix 1 input in
    let size, input = read_U32 input in
    let section_input = Input.sub_prefix size input in
    let next_input = Input.sub_suffix size input in
    let res, after_section_input = section_content_parse section_input in
    assert (Input.is_empty after_section_input);
    (res, next_input)
  | _ -> (default, input)

let vector parse_elt input =
  let nb_elt, input = read_U32 input in
  let rec loop loop_id input acc =
    if nb_elt = loop_id then (List.rev acc, input)
    else
      let acc_elt, input = parse_elt loop_id input in
      let acc = acc_elt :: acc in
      loop (loop_id + 1) input acc
  in
  loop 0 input []

let section_custom input =
  print_endline "custom";
  section_parse input ~expected_id:'\x00' () @@ consume_to_end ()

let section_type input =
  print_endline "type";
  section_parse input ~expected_id:'\x01' []
    (vector (fun id input ->
         let fcttype, input = read_byte input in
         assert (fcttype = '\x60');
         let params, input =
           vector (fun _id input -> deserialize_valtype input) input
         in
         let results, input =
           vector (fun _id input -> deserialize_valtype input) input
         in
         let params = List.map (fun param -> (None, param)) params in
         (Bt_raw (Some (Raw id), (params, results)), input) ) )

let section_import input =
  print_endline "import";
  section_parse input ~expected_id:'\x02' () @@ consume_to_end ()

let section_function input =
  print_endline "function";
  section_parse input ~expected_id:'\x03' []
  @@ vector (fun _id input -> deserialize_indice input)

let section_table input =
  print_endline "table";
  section_parse input ~expected_id:'\x04' Named.empty
  @@ consume_to_end Named.empty

let section_memory input =
  print_endline "memory";
  section_parse input ~expected_id:'\x05' []
  @@ vector (fun _id input ->
         let limits, input = deserialize_limits input in
         ((None, limits), input) )

let section_global input =
  print_endline "global";
  section_parse input ~expected_id:'\x06' []
  @@ vector (fun _id input ->
         let val_type, input = deserialize_valtype input in
         let mut, input = deserialize_mut input in
         let expr, input = deserialize_code input [] in
         ((expr, (mut, val_type)), input) )

let section_export input =
  print_endline "export";
  section_parse input ~expected_id:'\x07' []
  @@ vector (fun _id input ->
         let name, input = vector (fun _id input -> read_byte input) input in
         let name = String.of_seq (List.to_seq name) in
         let export_typeidx, input = read_byte input in
         let id, input = deserialize_indice input in
         ((export_typeidx, { id; name }), input) )

let section_start input =
  print_endline "start";
  section_parse input ~expected_id:'\x08' None @@ fun input ->
  let idx_start_func, input = deserialize_indice input in
  (Some idx_start_func, input)

let section_element input =
  print_endline "element";
  section_parse input ~expected_id:'\x09' Named.empty
  @@ consume_to_end Named.empty

let section_code input =
  print_endline "code";
  section_parse input ~expected_id:'\x0A' []
    (vector (fun _id input ->
         let _size, input = read_U32 input in
         let locals, input =
           vector (fun _id input -> deserialize_valtype input) input
         in
         let locals = List.map (fun loc -> (None, loc)) locals in
         let code, input = deserialize_code input [] in
         ((locals, code), input) ) )

let section_data input =
  print_endline "data";
  section_parse input ~expected_id:'\x0B' []
    (vector (fun _id input ->
         let i, input = read_U32 input in
         match i with
         | 0 ->
           let expr, input = deserialize_code input [] in
           let bytes, input = vector (fun _id input -> read_byte input) input in
           let init = String.of_seq (List.to_seq bytes) in
           ({ id = None; init; mode = Data_active (Some 0, expr) }, input)
         | 1 ->
           let bytes, input = vector (fun _id input -> read_byte input) input in
           let init = String.of_seq (List.to_seq bytes) in
           ({ id = None; init; mode = Data_passive }, input)
         | 2 ->
           let memidx, input = deserialize_indice input in
           let expr, input = deserialize_code input [] in
           let bytes, input = vector (fun _id input -> read_byte input) input in
           let init = String.of_seq (List.to_seq bytes) in
           ({ id = None; init; mode = Data_active (Some memidx, expr) }, input)
         | _ -> failwith "section_data error" ) )

let sections_iterate (modul : Simplified.modul) (input : Input.t) =
  let (), input = section_custom input in
  let block_type_list_type, input = section_type input in
  let (), input = section_import input in
  let typeidx_list_func, input = section_function input in
  let table, input = section_table input in
  let mems, input = section_memory input in
  let globals, input = section_global input in
  let exports, input = section_export input in
  let start, input = section_start input in
  let elem, input = section_element input in
  let locals_code_list, input = section_code input in
  let datas, input = section_data input in
  assert (Input.is_empty input);
  let block_type_list_type = Array.of_list block_type_list_type in
  let mem =
    let values =
      List.mapi (fun id mem -> Indexed.return id (Runtime.Local mem)) mems
    in
    { Named.values; named = String_map.empty }
  in
  let global =
    let values =
      List.mapi
        (fun id (expr, global) ->
          Indexed.return id
            (Runtime.Local { typ = global; init = expr; id = None }) )
        globals
    in
    { Named.values; named = String_map.empty }
  in
  let func =
    let values =
      let id = ref (-1) in
      List.map2
        (fun typeidx (locals, body) ->
          incr id;
          Indexed.return !id
            (Runtime.Local
               { type_f = Array.get block_type_list_type typeidx
               ; locals
               ; body
               ; id = None
               } ) )
        typeidx_list_func locals_code_list
    in
    { Named.values; named = String_map.empty }
  in
  let data =
    let values = List.mapi (fun id data -> Indexed.return id data) datas in
    { Named.values; named = String_map.empty }
  in
  let empty_exports = { global = []; mem = []; table = []; func = [] } in
  let exports =
    List.fold_left
      (fun (exports : exports) (export_typeidx, export) ->
        match export_typeidx with
        | '\x00' ->
          let func = export :: exports.func in
          { exports with func }
        | '\x01' ->
          let table = export :: exports.table in
          { exports with table }
        | '\x02' ->
          let mem = export :: exports.mem in
          { exports with mem }
        | '\x03' ->
          let global = export :: exports.global in
          { exports with global }
        | _ -> failwith "deserialize_exportdesc error" )
      empty_exports exports
  in
  let modul =
    { modul with global; mem; elem; func; table; start; data; exports }
  in
  Ok modul

let empty_modul () : Simplified.modul =
  { id = None
  ; global = Named.empty
  ; table = Named.empty
  ; mem = Named.empty
  ; func = Named.empty
  ; elem = Named.empty
  ; data = Named.empty
  ; exports = { global = []; mem = []; table = []; func = [] }
  ; start = None
  }

let deserialize content =
  let bytes = Bytes.of_string content in
  let* () = magic_check bytes in
  let* () = version_check bytes in
  let modul = empty_modul () in
  let input = Input.from_bytes bytes |> Input.sub_suffix 8 in
  sections_iterate modul input

let from_file (filename : Fpath.t) =
  let filename = Fpath.filename filename in
  let ic = In_channel.open_bin filename in
  let content = In_channel.input_all ic in
  In_channel.close ic;
  deserialize content
