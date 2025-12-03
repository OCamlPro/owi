  $ owi sym global.wat --deterministic-result-order --exploration=lifo
  All OK!
  $ owi sym global.wat --deterministic-result-order --exploration=fifo
  All OK!
  $ owi sym global.wat --deterministic-result-order --exploration=random
  All OK!
  $ owi sym global.wat --deterministic-result-order --exploration=smart 2>&1 | grep -v "line"
  owi: internal error, uncaught exception:
