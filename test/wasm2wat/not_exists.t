  $ owi wasm2wat idontexist.wat
  Usage: owi wasm2wat [--help] [--emit-file] [--output=FILE] [OPTION]… FILE
  owi: FILE argument: no file 'idontexist.wat'
  [124]
  $ owi wasm2wat bad.ext
  owi: [ERROR] unsupported file_extension ".ext"
  [54]
