  $ owi wasm of_wat not_exists.wat
  Usage: owi wasm of_wat [--help] [--output=FILE] [--unsafe] [OPTION]… FILE
  owi: FILE argument: no file 'not_exists.wat'
  [124]
  $ owi wasm of_wat bad.ext
  owi: [ERROR] unsupported file_extension ".ext"
  [54]
