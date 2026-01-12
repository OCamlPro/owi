  $ owi wasm2wat idontexist.wat
  owi: FILE argument: no file 'idontexist.wat'
  Usage: owi wasm2wat [--emit-file] [--output=FILE] [OPTION]… FILE
  Try 'owi wasm2wat --help' or 'owi --help' for more information.
  [124]
  $ owi wasm2wat bad.ext
  owi: [ERROR] unsupported file_extension ".ext"
  [54]
