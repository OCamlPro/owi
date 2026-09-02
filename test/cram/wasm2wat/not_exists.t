  $ owi wasm to_wat idontexist.wat
  Usage: owi wasm to_wat [--help] [--emit-file] [--output=FILE] [OPTION]…
         FILE
  owi: FILE argument: no file 'idontexist.wat'
  [124]
  $ owi wasm to_wat bad.ext
  owi: [ERROR] unsupported file_extension ".ext"
  [54]
