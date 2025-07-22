  $ owi analyze cg ./call.wat
  $ cat call.dot
  digraph call_graph {
   0 [label="start"];
   0 -> 1 ;
   0 -> 2 ;
   1 [label="a"];
   1 -> 2 ;
   1 -> 3 ;
   2 [label="b"];
   3 [label="c"];
   5 [label="e"];
   }

  $ owi analyze cg ./call.wat --entry-point=a
  $ cat call.dot
  digraph call_graph {
   1 [label="a"];
   1 -> 2 ;
   1 -> 3 ;
   2 [label="b"];
   3 [label="c"];
   }

  $ owi analyze cg ./indirect_call.wat --call-graph-mode=sound
  $ cat indirect_call.dot
  digraph call_graph {
   1 [label="f"];
   4 [label="main"];
   4 -> 1 ;
   }

  $ owi analyze cg ./indirect_call.wat --call-graph-mode=complete
  $ cat indirect_call.dot
  digraph call_graph {
   1 [label="f"];
   2 [label="g"];
   3 [label="h"];
   4 [label="main"];
   4 -> 1 ;
   4 -> 2 ;
   4 -> 3 ;
   }
