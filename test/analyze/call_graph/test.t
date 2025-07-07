  $ owi analyze cg ./call.wat
  $ cat call.dot
  digraph call_graph {
  5;0;
  0 -> 2;
  1 -> 3;
  1 -> 2;
  0 -> 1}

  $ owi analyze cg ./call.wat --entry-point=a
  $ cat call.dot
  digraph call_graph {
  1;
  1 -> 3;
  1 -> 2}

  $ owi analyze cg ./indirect_call.wat --call-graph-mode=sound
  $ cat indirect_call.dot
  digraph call_graph {
  4;
  4 -> 1}

  $ owi analyze cg ./indirect_call.wat --call-graph-mode=complete
  $ cat indirect_call.dot
  digraph call_graph {
  4;
  4 -> 3;
  4 -> 2;
  4 -> 1}
