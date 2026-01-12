  $ owi wat2wasm not_exists.wat
  owi: FILE argument: no file 'not_exists.wat'
  Usage: owi wat2wasm [--output=FILE] [--unsafe] [OPTION]… FILE
  Try 'owi wat2wasm --help' or 'owi --help' for more information.
  [124]
  $ owi wat2wasm bad.ext
  owi: [ERROR] unsupported file_extension ".ext"
  [54]
