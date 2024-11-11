let clock : Shared.LoxFunction.t Callable.callable =
  object
    method arity = 0
    method call _ _ = Shared.Expr.Num 0.0
    method to_string = "<native fn>"
  end
