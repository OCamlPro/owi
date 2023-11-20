open Types

val exec_expr :
  Link_env.Build.t -> simplified Const.expr -> Concrete_value.t Result.t
