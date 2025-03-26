no subcommand should print help
  $ owi
  NAME
         owi - OCaml WebAssembly Interpreter
  
  SYNOPSIS
         owi [COMMAND] …
  
  COMMANDS
         c [OPTION]… FILE…
             Compile a C file to Wasm and run the symbolic interpreter on it
  
         c++ [OPTION]… FILE…
             Compile a C++ file to Wasm and run the symbolic interpreter on it
  
         conc [OPTION]… FILE…
             Run the concolic interpreter
  
         fmt [--inplace] [OPTION]… FILE…
             Format a .wat or .wast file
  
         instrument [--debug] [--symbolic] [--unsafe] [OPTION]… FILE…
             Generate an instrumented file with runtime assertion checking
             coming from Weasel specifications
  
         opt [--debug] [--output=FILE] [--unsafe] [OPTION]… FILE
             Optimize a module
  
         replay [OPTION]… FILE
             Replay a module containing symbols with concrete values in a
             replay file containing a model
  
         run [OPTION]… FILE…
             Run the concrete interpreter
  
         rust [OPTION]… FILE…
             Compile a Rust file to Wasm and run the symbolic interpreter on it
  
         script [OPTION]… FILE…
             Run a reference test suite script
  
         sym [OPTION]… FILE…
             Run the symbolic interpreter
  
         validate [--debug] [OPTION]… FILE…
             Validate a module
  
         version [OPTION]…
             Print some version informations
  
         wasm2wat [--emit-file] [--output=FILE] [OPTION]… FILE
             Generate a text format file (.wat) from a binary format file
             (.wasm)
  
         wat2wasm [OPTION]… FILE
             Generate a binary format file (.wasm) from a text format file
             (.wat)
  
         zig [OPTION]… FILE…
             Compile a Zig file to Wasm and run the symbolic interpreter on it
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         owi exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Email them to <contact@ndrs.fr>.
  
