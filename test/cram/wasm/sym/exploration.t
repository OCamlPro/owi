  $ owi wasm sym global.wat --deterministic-result-order --exploration=lifo
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=fifo
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=random
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=random-unseen-then-random
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=rarity
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=hot-path-penalty
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=rarity-aging
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=rarity-depth-aging
  All OK!
  $ owi wasm sym global.wat --deterministic-result-order --exploration=rarity-depth-loop-aging
  All OK!
