  $ owi cfg test_cfg.wat
  $ cat test_cfg.dot
  digraph cfg {
   rankdir=LR;
   node [shape=record];
   0 [label="local.get 0 | i32.eqz | br_if 0"];
  1 [label="local.get 0 | i32.const 1 | i32.eq | br_if 1"];
  2 [label="i32.const 7 | local.set 1 | br 2"];
  3 [label="i32.const 42 | local.set 1 | unreachable"];
  4 [label="i32.const 99 | local.set 1"];
  4 [label="local.get 1"];
   0 -> 1 [label="false"];
  1 -> 2 [label="false"];
  0 -> 3 [label="true"];
  1 -> 4 [label="true"];
  2 -> 4 }

  $ owi cfg loop.wat
  $ cat loop.dot
  digraph cfg {
   rankdir=LR;
   node [shape=record];
   0 [label="i64.const 1 | local.set 1 | local.get 0 | i64.eqz | br_if 0"];
  1 [label="loop"];
  2 [label="local.get 1 | local.get 0 | i64.mul | local.set 1 | local.get 0 | i64.const -1 | i64.add | local.tee 0 | i64.eqz | br_if 1"];
  3 [label="br 0"];
  4 [label=""];
  4 [label="local.get 1"];
   0 -> 1 [label="false"];
  1 -> 2 ;
  2 -> 3 [label="false"];
  3 -> 1 ;
  0 -> 4 [label="true"];
  2 -> 4 [label="true"]}

  $ owi cfg fib.wat
  $ cat fib.dot
  digraph cfg {
   rankdir=LR;
   node [shape=record];
   0 [label="local.get 0 | i32.const 2 | i32.lt_u | if"];
  1 [label="local.get 0 | return"];
  2 [label=""];
  3 [label="local.get 0 | i32.const 1 | i32.sub | i32.const 4 | i32.mul | i32.load align=1 | local.tee 1 | if"];
  4 [label=""];
  5 [label="local.get 0 | i32.const 1 | i32.sub | i32.const 4 | i32.mul | local.get 0 | i32.const 1 | i32.sub | call 0 | local.tee 1 | i32.store align=1"];
  6 [label="local.get 1 | local.get 0 | i32.const 2 | i32.sub | i32.const 4 | i32.mul | i32.load align=1 | local.tee 2 | if"];
  7 [label=""];
  8 [label="local.get 0 | i32.const 2 | i32.sub | i32.const 4 | i32.mul | local.get 0 | i32.const 2 | i32.sub | call 0 | local.tee 2 | i32.store align=1"];
  9 [label="local.get 2 | i32.add | return"];
   0 -> 1 [label="true"];
  0 -> 2 [label="false"];
  2 -> 3 ;
  3 -> 4 [label="true"];
  3 -> 5 [label="false"];
  5 -> 6 ;
  4 -> 6 ;
  6 -> 7 [label="true"];
  6 -> 8 [label="false"];
  8 -> 9 ;
  7 -> 9 }
