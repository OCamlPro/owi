  $ owi wat2wasm not_exists.wat
  Usage: owi wat2wasm [--help] [--output=FILE] [--unsafe] [OPTION]… FILE
  owi: FILE argument: no file 'not_exists.wat'
  [124]
  $ owi wat2wasm bad.ext
  owi: [ERROR] unsupported file_extension ".ext"
  [54]
