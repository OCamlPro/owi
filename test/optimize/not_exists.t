not exists file (exists_not.wast instead of not_exists.wast):
  $ dune exec -- owi opt exists_not.wast --debug
  Error: file `exists_not.wast` doesn't exist
  [1]
