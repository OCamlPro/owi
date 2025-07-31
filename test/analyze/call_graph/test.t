  $ owi analyze cg ./call.wat
  $ cat call.dot
  digraph call_graph {
    0
    0 -> 1
    0 -> 2
    1
    1 -> 2
    1 -> 3
    2
    
    3
    
    5
    }

  $ owi analyze cg ./call.wat --entry-point=a
  $ cat call.dot
  digraph call_graph {
    1
    1 -> 2
    1 -> 3
    2
    
    3
    }

  $ owi analyze cg ./indirect_call.wat --call-graph-mode=sound
  $ cat indirect_call.dot
  digraph call_graph {
    1
    
    4
    4 -> 1}

  $ owi analyze cg ./indirect_call.wat --call-graph-mode=complete
  $ cat indirect_call.dot
  digraph call_graph {
    1
    
    2
    
    3
    
    4
    4 -> 1
    4 -> 2
    4 -> 3}
