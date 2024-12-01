let clock : Shared.LoxFunction.t Callable.callable =
  object
    method arity = 0

    method call _ _ =
      Shared.Expr.Num
        Core.Time_float.(now () |> to_span_since_epoch |> Span.to_sec)

    method to_string = "<native fn>"
  end
