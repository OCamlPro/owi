type logger =
  { channel : out_channel
  ; mutex : Mutex.t
  }

let global_stats_logger : logger option ref = ref None

let init_logger_to_file f =
  let logger =
    { channel = Out_channel.open_text (Fpath.to_string f)
    ; mutex = Mutex.create ()
    }
  in
  Out_channel.output_char logger.channel '[';
  global_stats_logger := Some logger

(* Be careful that f will be run in the critical section
   so should be kept small *)
let on_logger f =
  match !global_stats_logger with
  | None -> ()
  | Some logger -> begin
    Mutex.protect logger.mutex (fun () -> f logger.channel)
  end

(* assumes that v does not need escaping*)
let emit_key_val_s ch ?(end_comma = true) k v =
  Out_channel.output_char ch '"';
  Out_channel.output_string ch k;
  Out_channel.output_char ch '"';
  Out_channel.output_string ch {|:"|};
  Out_channel.output_string ch v;
  Out_channel.output_char ch '"';
  if end_comma then Out_channel.output_char ch ','

(* assumes that v does not need escaping*)
let emit_key_val_i ch ?(end_comma = true) k v =
  Out_channel.output_char ch '"';
  Out_channel.output_string ch k;
  Out_channel.output_char ch '"';
  Out_channel.output_char ch ':';
  Out_channel.output_string ch @@ Int.to_string v;
  if end_comma then Out_channel.output_char ch ','

type scope =
  | Global
  | Process
  | Thread

let event ?(scope = Thread) ?(arg_writter = None) name cat =
  let pid = Unix.getpid () in
  let tid = (Domain.self () :> int) in
  let ts = Int.of_float (Unix.gettimeofday () *. 1e6) in
  let scope =
    match scope with Global -> "g" | Process -> "p" | Thread -> "t"
  in
  on_logger (fun ch ->
      begin
        Out_channel.output_string ch "{";
        emit_key_val_s ch "name" name;
        emit_key_val_s ch "cat" cat;
        emit_key_val_s ch "ph" "i";
        emit_key_val_i ch "ts" ts;
        emit_key_val_i ch "pid" pid;
        emit_key_val_i ch "tid" tid;
        emit_key_val_s ch "s" scope ~end_comma:false;
        begin
          match arg_writter with
          | None -> ()
          | Some arg_writter -> begin
            Out_channel.output_string ch {|,"args":{|};
            arg_writter ch;
            Out_channel.output_char ch '}'
          end
        end;
        Out_channel.output_string ch "},"
      end )

let start_span ?(arg_writter = None) name cat =
  let pid = Unix.getpid () in
  let tid = (Domain.self () :> int) in
  let ts = Int.of_float (Unix.gettimeofday () *. 1e6) in
  on_logger (fun ch ->
      begin
        Out_channel.output_string ch "{";
        emit_key_val_s ch "name" name;
        emit_key_val_s ch "cat" cat;
        emit_key_val_s ch "ph" "B";
        emit_key_val_i ch "ts" ts;
        emit_key_val_i ch "pid" pid;
        emit_key_val_i ch "tid" tid ~end_comma:false;
        begin
          match arg_writter with
          | None -> ()
          | Some arg_writter -> begin
            Out_channel.output_string ch {|,"args":{|};
            arg_writter ch;
            Out_channel.output_char ch '}'
          end
        end;
        Out_channel.output_string ch "},"
      end )

let close_span () =
  let pid = Unix.getpid () in
  let tid = (Domain.self () :> int) in
  let ts = Int.of_float (Unix.gettimeofday () *. 1e6) in
  on_logger (fun ch ->
      begin
        Out_channel.output_string ch "{";
        emit_key_val_s ch "ph" "E";
        emit_key_val_i ch "ts" ts;
        emit_key_val_i ch "pid" pid;
        emit_key_val_i ch "tid" tid ~end_comma:false;
        Out_channel.output_string ch "},"
      end )

(* {
   "name": "myName",
   "cat": "category,list",
   "ph": "B",
   "ts": 12345,
   "pid": 123,
   "tid": 456,
   "args": {
     "someArg": 1,
     "anotherArg": {
       "value": "my value"
     }
   } *)
(* } *)
