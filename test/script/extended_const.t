  $ owi script --no-exhaustion reference/proposals/extended-const/data.wast
  $ owi script --no-exhaustion reference/proposals/extended-const/elem.wast
  got:      i32.const 65
  expected: (i32.const 66)
  bad result
  [3]
  $ owi script --no-exhaustion reference/proposals/extended-const/global.wast
