no subcommand should print help
  $ owi
  NAME
         owi - Seamless program analysis for C, C++, Go, Rust, Wasm and Zig.
  
  SYNOPSIS
         owi [COMMAND] …
  
  COMMANDS
         c [OPTION]… FILE…
             Compile a C file to Wasm and run the symbolic interpreter on it
  
         c++ [OPTION]… FILE…
             Compile a C++ file to Wasm and run the symbolic interpreter on it
  
         haskell [OPTION]… FILE…
             Compile a Haskell file to Wasm and run the symbolic interpreter on
             it
  
         llvm [OPTION]… FILE…
             Compile LLVM IR/bitcode to Wasm and run the symbolic interpreter
             on it
  
         rust [OPTION]… FILE…
             Compile a Rust file to Wasm and run the symbolic interpreter on it
  
         tinygo [OPTION]… FILE…
             Compile a TinyGo file to Wasm and run the symbolic interpreter on
             it
  
         version [OPTION]…
             Print some version informations
  
         wasm [COMMAND] …
             Work with Wasm programs.
  
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
         Email them to <owi.wildcat119@passmail.com>.
  

