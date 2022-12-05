val until_link :
     Link.state
  -> Types.module_
  -> (Link.module_to_run * Link.state, string) result

val until_interpret : Link.state -> Types.module_ -> (Link.state, string) result
