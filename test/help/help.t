no subcommand should print help
  $ owi
  NAME
         owi - OCaml WebAssembly Interpreter
  
  SYNOPSIS
         owi [COMMAND] …
  
  COMMANDS
         c [OPTION]… [ARG]…
             Compile a C file to Wasm and run the symbolic interpreter on it
  
         conc [OPTION]… [ARG]…
             Run the concolic interpreter
  
         fmt [--inplace] [OPTION]… [ARG]…
             Format a .wat or .wast file
  
         opt [--debug] [--unsafe] [OPTION]… [ARG]…
             Optimize a module
  
         run [OPTION]… [ARG]…
             Run the concrete interpreter
  
         script [OPTION]… [ARG]…
             Run a reference test suite script
  
         sym [OPTION]… [ARG]…
             Run the symbolic interpreter
  
         validate [--debug] [OPTION]… [ARG]…
             Validate a module
  
         wasm2wat [OPTION]… [ARG]…
             Generate a text format file (.wat) file from a binary format file
             (.wasm)
  
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
  
