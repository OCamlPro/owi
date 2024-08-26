  $ owi wasm2wat idontexist.wat
  owi: no file 'idontexist.wat'
  Usage: owi wasm2wat [--emit-file] [--output=FILE] [OPTION]… ARG
  Try 'owi wasm2wat --help' or 'owi --help' for more information.
  [124]
  $ owi wasm2wat bad.ext
  unsupported file_extension ".ext"
  [53]
