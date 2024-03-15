(* binary format specification:
   https://webassembly.github.io/spec/core/binary/modules.html#binary-importsec *)

open Simplified
open Syntax
open Types
open Binary_basic_decode

type ('a, 'b) import =
  | Func of int
  | Table of limits * 'a ref_type
  | Mem of limits
  | Global of mut * 'b val_type

let consume_to_end x error_msg_info input =
  let+ input = Input.sub ~pos:0 ~len:0 error_msg_info input in
  (x, input)

let magic_check str =
  let magic = String.sub str 0 4 in
  if String.equal magic "\x00\x61\x73\x6d" then Ok ()
  else Error (`Msg "magic_check error")

let version_check str =
  let version = String.sub str 4 4 in
  if String.equal version "\x01\x00\x00\x00" then Ok ()
  else Error (`Msg "version_check error")

let section_parse input error_msg_info ~expected_id default
  section_content_parse =
  if Input.is_empty input then Ok (default, input)
  else
    let* id =
      Error.add_context ("section_parse " ^ error_msg_info) @@ Input.get0 input
    in
    if id = expected_id then (
      let* input = Input.sub_suffix 1 error_msg_info input in
      let* size, input = read_U32 input in
      let* section_input = Input.sub_prefix size error_msg_info input in
      let* next_input = Input.sub_suffix size error_msg_info input in
      let* res, after_section_input = section_content_parse section_input in
      assert (Input.is_empty after_section_input);
      Ok (res, next_input) )
    else Ok (default, input)

let section_custom input =
  section_parse input "custom_section" ~expected_id:'\x00' ()
  @@ consume_to_end () "custom_section"

let section_type input =
  section_parse input "type_section" ~expected_id:'\x01' []
    (vector (fun id input ->
         let* fcttype, input = read_byte input in
         assert (fcttype = '\x60');
         let* params, input = vector_no_id deserialize_valtype input in
         let+ results, input = vector_no_id deserialize_valtype input in
         let params = List.map (fun param -> (None, param)) params in
         (Bt_raw (Some (Raw id), (params, results)), input) ) )

let section_import input =
  section_parse input "import_section" ~expected_id:'\x02' []
  @@ vector_no_id (fun input ->
         let* modul, input = vector_no_id read_byte input in
         let* name, input = vector_no_id read_byte input in
         let modul = string_of_char_list modul in
         let name = string_of_char_list name in
         let* import_typeidx, input = read_byte input in
         match import_typeidx with
         | '\x00' ->
           let* typeidx, input = read_U32 input in
           Ok ((modul, name, Func typeidx), input)
         | '\x01' ->
           let* ref_type, input = deserialize_reftype input in
           let* limits, input = deserialize_limits input in
           Ok ((modul, name, Table (limits, ref_type)), input)
         | '\x02' ->
           let* limits, input = deserialize_limits input in
           Ok ((modul, name, Mem limits), input)
         | '\x03' ->
           let* val_type, input = deserialize_valtype input in
           let* mut, input = deserialize_mut input in
           Ok ((modul, name, Global (mut, val_type)), input)
         | c ->
           Error.fail (Input.global_pos input)
             (Format.sprintf "import_section error: char %c" c) )

let section_function input =
  section_parse input "function_section" ~expected_id:'\x03' []
  @@ vector_no_id read_U32

let section_table input =
  section_parse input "table_section" ~expected_id:'\x04' []
  @@ vector_no_id (fun input ->
         let* ref_type, input = deserialize_reftype input in
         let+ limits, input = deserialize_limits input in
         ((limits, ref_type), input) )

let section_memory input =
  section_parse input "memory_section" ~expected_id:'\x05' []
  @@ vector_no_id (fun input ->
         let+ limits, input = deserialize_limits input in
         ((None, limits), input) )

let section_global block_type_array input =
  section_parse input "global_section" ~expected_id:'\x06' []
  @@ vector_no_id (fun input ->
         let* val_type, input = deserialize_valtype input in
         let* mut, input = deserialize_mut input in
         let+ expr, input = deserialize_code block_type_array [] input in
         ((expr, (mut, val_type)), input) )

let section_export input =
  section_parse input "export_section" ~expected_id:'\x07' []
  @@ vector_no_id (fun input ->
         let* name, input = vector_no_id read_byte input in
         let name = string_of_char_list name in
         let* export_typeidx, input = read_byte input in
         let+ id, input = read_U32 input in
         ((export_typeidx, { id; name }), input) )

let section_start input =
  section_parse input "start_section" ~expected_id:'\x08' None @@ fun input ->
  let+ idx_start_func, input = read_U32 input in
  (Some idx_start_func, input)

let section_element block_type_array input =
  section_parse input "element_section" ~expected_id:'\x09' []
    (vector_no_id (fun input ->
         let* i, input = read_U32 input in
         match i with
         | 0 ->
           let* expr, input = deserialize_code block_type_array [] input in
           let* funcidx_l, input = vector_no_id deserialize_indice input in
           let init =
             List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
           in
           Ok
             ( { id = None
               ; typ = (Null, Func_ht)
               ; init
               ; mode = Elem_active (Some 0, expr)
               }
             , input )
         | 1 ->
           let* elemkind, input = read_byte input in
           begin
             match elemkind with
             | '\x00' ->
               let* funcidx_l, input = vector_no_id deserialize_indice input in
               let init =
                 List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
               in
               Ok
                 ( { id = None
                   ; typ = (Null, Func_ht)
                   ; init
                   ; mode = Elem_passive
                   }
                 , input )
             | c ->
               Error.fail (Input.global_pos input)
                 (Format.sprintf "element_section 1 error: char %c" c)
           end
         | 2 ->
           let* Raw tableidx, input = deserialize_indice input in
           let* expr, input = deserialize_code block_type_array [] input in
           let* elemkind, input = read_byte input in
           begin
             match elemkind with
             | '\x00' ->
               let* funcidx_l, input = vector_no_id deserialize_indice input in
               let init =
                 List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
               in
               Ok
                 ( { id = None
                   ; typ = (Null, Func_ht)
                   ; init
                   ; mode = Elem_active (Some tableidx, expr)
                   }
                 , input )
             | c ->
               Error.fail (Input.global_pos input)
                 (Format.sprintf "element_section 2 error: char %c" c)
           end
         | 3 ->
           let* elemkind, input = read_byte input in
           begin
             match elemkind with
             | '\x00' ->
               let* funcidx_l, input = vector_no_id deserialize_indice input in
               let init =
                 List.map (fun funcidx -> [ Ref_func funcidx ]) funcidx_l
               in
               Ok
                 ( { id = None
                   ; typ = (Null, Func_ht)
                   ; init
                   ; mode = Elem_declarative
                   }
                 , input )
             | c ->
               Error.fail (Input.global_pos input)
                 (Format.sprintf "element_section 3 error: char %c" c)
           end
         | 4 ->
           let* expr, input = deserialize_code block_type_array [] input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok
             ( { id = None
               ; typ = (Null, Func_ht)
               ; init
               ; mode = Elem_active (Some 0, expr)
               }
             , input )
         | 5 ->
           let* typ, input = deserialize_reftype input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok ({ id = None; typ; init; mode = Elem_passive }, input)
         | 6 ->
           let* Raw tableidx, input = deserialize_indice input in
           let* expr, input = deserialize_code block_type_array [] input in
           let* typ, input = deserialize_reftype input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok
             ( { id = None; typ; init; mode = Elem_active (Some tableidx, expr) }
             , input )
         | 7 ->
           let* typ, input = deserialize_reftype input in
           let* init, input =
             vector_no_id
               (fun input -> deserialize_code block_type_array [] input)
               input
           in
           Ok ({ id = None; typ; init; mode = Elem_declarative }, input)
         | i ->
           Error.fail (Input.global_pos input)
             (Format.sprintf "element_section %d error" i) ) )

let section_code block_type_array input =
  section_parse input "code_section" ~expected_id:'\x0A' []
    (vector_no_id (fun input ->
         let* _size, input = read_U32 input in
         let* locals, input =
           vector_no_id
             (fun input ->
               let* nb, input = read_U32 input in
               let+ vt, input = deserialize_valtype input in
               (List.init nb (fun _ -> (None, vt)), input) )
             input
         in
         let locals = List.flatten locals in
         let+ code, input = deserialize_code block_type_array [] input in
         ((locals, code), input) ) )

let section_data block_type_array input =
  section_parse input "data_section" ~expected_id:'\x0B' []
    (vector_no_id (fun input ->
         let* i, input = read_U32 input in
         match i with
         | 0 ->
           let* expr, input = deserialize_code block_type_array [] input in
           let* bytes, input = vector_no_id read_byte input in
           let init = string_of_char_list bytes in
           Ok ({ id = None; init; mode = Data_active (Some 0, expr) }, input)
         | 1 ->
           let* bytes, input = vector_no_id read_byte input in
           let init = string_of_char_list bytes in
           Ok ({ id = None; init; mode = Data_passive }, input)
         | 2 ->
           let* memidx, input = read_U32 input in
           let* expr, input = deserialize_code block_type_array [] input in
           let* bytes, input = vector_no_id read_byte input in
           let init = string_of_char_list bytes in
           Ok
             ({ id = None; init; mode = Data_active (Some memidx, expr) }, input)
         | i ->
           Error.fail (Input.global_pos input)
             (Format.sprintf "data_section %d error" i) ) )

let sections_iterate (modul : Simplified.modul) (input : Input.t) =
  let* (), input = section_custom input in
  let* block_type_list_type, input = section_type input in
  let block_type_array = Array.of_list block_type_list_type in
  let* imports, input = section_import input in
  let* typeidx_list_func, input = section_function input in
  let* tables, input = section_table input in
  let* mems, input = section_memory input in
  let* globals, input = section_global block_type_array input in
  let* exports, input = section_export input in
  let* start, input = section_start input in
  let* elems, input = section_element block_type_array input in
  let* locals_code_list, input = section_code block_type_array input in
  let+ datas, input = section_data block_type_array input in
  assert (Input.is_empty input);
  let mem =
    let values =
      List.mapi (fun id mem -> Indexed.return id (Runtime.Local mem)) mems
    in
    let id = ref (List.length mems - 1) in
    let values =
      values
      @ List.filter_map
          (fun (modul, name, type_import) ->
            match type_import with
            | Mem limits ->
              incr id;
              Some
                (Indexed.return !id
                   (Runtime.Imported
                      { modul; name; assigned_name = None; desc = limits } ) )
            | _ -> None )
          imports
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
    let id = ref (List.length globals - 1) in
    let values =
      values
      @ List.filter_map
          (fun (modul, name, type_import) ->
            match type_import with
            | Global (mut, val_type) ->
              incr id;
              Some
                (Indexed.return !id
                   (Runtime.Imported
                      { modul
                      ; name
                      ; assigned_name = None
                      ; desc = (mut, val_type)
                      } ) )
            | _ -> None )
          imports
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
               { type_f = Array.get block_type_array typeidx
               ; locals
               ; body
               ; id = None
               } ) )
        typeidx_list_func locals_code_list
    in
    let id = ref (List.length locals_code_list - 1) in
    let values =
      values
      @ List.filter_map
          (fun (modul, name, type_import) ->
            match type_import with
            | Func typeidx ->
              incr id;
              Some
                (Indexed.return !id
                   (Runtime.Imported
                      { modul
                      ; name
                      ; assigned_name = None
                      ; desc = Array.get block_type_array typeidx
                      } ) )
            | _ -> None )
          imports
    in
    { Named.values; named = String_map.empty }
  in
  let table =
    let values =
      List.mapi
        (fun id tbl -> Indexed.return id (Runtime.Local (None, tbl)))
        tables
    in
    let id = ref (-1) in
    let values =
      values
      @ List.filter_map
          (fun (modul, name, type_import) ->
            match type_import with
            | Table (limits, ref_type) ->
              incr id;
              Some
                (Indexed.return !id
                   (Runtime.Imported
                      { modul
                      ; name
                      ; assigned_name = None
                      ; desc = (limits, ref_type)
                      } ) )
            | _ -> None )
          imports
    in
    { Named.values; named = String_map.empty }
  in
  let elem =
    let values = List.mapi (fun id elem -> Indexed.return id elem) elems in
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
  { modul with global; mem; elem; func; table; start; data; exports }

let deserialize content =
  let* () = magic_check content in
  let* () = version_check content in
  let input =
    Input.from_str_bytes content "full_file" |> Input.sub_suffix 8 "full_file"
  in
  match input with
  | Ok input ->
    let modul = sections_iterate Simplified.empty_modul input in
    begin
      match modul with
      | Ok _ as m -> m
      | Error err -> Error (`Msg (Error.msg err))
    end
  | Error err -> Error (`Msg (Error.msg err))

let from_file (filename : Fpath.t) =
  let* res =
    Bos.OS.File.with_ic filename
      (fun chan () ->
        let content = In_channel.input_all chan in
        deserialize content )
      ()
  in
  res
