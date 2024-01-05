not exists file (exists_not.wast instead of not_exists.wast):
  $ dune exec owi -- sym exists_not.wast
  Error: file `exists_not.wast` doesn't exist
  [1]
