  $ owi analyze cfg test_cfg.wat
  $ cat test_cfg.dot
  digraph cfg {
    rankdir=LR;
    node [shape=record];
    0 [label="local.get 0 | i32.eqz | br_if 0"]
    0 -> 1[label="false"]
    0 -> 3[label="true"]
    1 [label="local.get 0 | i32.const 1 | i32.eq | br_if 1"]
    1 -> 2[label="false"]
    1 -> 4[label="true"]
    2 [label="i32.const 7 | local.set 1 | br 2"]
    2 -> 5
    3 [label="i32.const 42 | local.set 1 | unreachable"]
    
    4 [label="i32.const 99 | local.set 1"]
    4 -> 5
    5 [label="local.get 1"]
    5 -> 6
    6 [label="return"]
    }

  $ owi analyze cfg loop.wat
  $ cat loop.dot
  digraph cfg {
    rankdir=LR;
    node [shape=record];
    0 [label="i64.const 1 | local.set 1 | local.get 0 | i64.eqz | br_if 0"]
    0 -> 1[label="false"]
    0 -> 3[label="true"]
    1 [label="loop | local.get 1 | local.get 0 | i64.mul | local.set 1 | local.get 0 | i64.const -1 | i64.add | local.tee 0 | i64.eqz | br_if 1"]
    1 -> 2[label="false"]
    1 -> 3[label="true"]
    2 [label="br 0"]
    2 -> 1
    3 [label="local.get 1"]
    3 -> 4
    4 [label="return"]
    }

  $ owi analyze cfg fib.wat
  $ cat fib.dot
  digraph cfg {
    rankdir=LR;
    node [shape=record];
    0 [label="local.get 0 | i32.const 2 | i32.lt_u | if"]
    0 -> 1[label="true"]
    0 -> 2[label="false"]
    1 [label="local.get 0"]
    1 -> 12
    2 [label=""]
    2 -> 3
    3 [label="local.get 0 | i32.const 1 | i32.sub | i32.const 4 | i32.mul | i32.load align=1 | local.tee 1 | if"]
    3 -> 4[label="true"]
    3 -> 5[label="false"]
    4 [label=""]
    4 -> 7
    5 [label="local.get 0 | i32.const 1 | i32.sub | i32.const 4 | i32.mul | local.get 0 | i32.const 1 | i32.sub | call 0"]
    5 -> 6
    6 [label="local.tee 1 | i32.store align=1"]
    6 -> 7
    7 [label="local.get 1 | local.get 0 | i32.const 2 | i32.sub | i32.const 4 | i32.mul | i32.load align=1 | local.tee 2 | if"]
    7 -> 8[label="true"]
    7 -> 9[label="false"]
    8 [label=""]
    8 -> 11
    9 [label="local.get 0 | i32.const 2 | i32.sub | i32.const 4 | i32.mul | local.get 0 | i32.const 2 | i32.sub | call 0"]
    9 -> 10
    10 [label="local.tee 2 | i32.store align=1"]
    10 -> 11
    11 [label="local.get 2 | i32.add"]
    11 -> 12
    12 [label="return"]
    }

  $ owi analyze cfg br_table.wat
  $ cat br_table.dot
  digraph cfg {
    rankdir=LR;
    node [shape=record];
    0 [label="local.get 0 | br_table 1 0 2"]
    0 -> 1[label="1"]
    0 -> 3[label="0"]
    0 -> 4[label="default"]
    1 [label="local.get 1 | global.set 0 | local.get 1 | i32.const 42 | i32.eq | br_if 0"]
    1 -> 2[label="false"]
    1 -> 3[label="true"]
    2 [label=""]
    2 -> 4
    3 [label="local.get 1 | global.set 1"]
    3 -> 4
    4 [label="return"]
    }
