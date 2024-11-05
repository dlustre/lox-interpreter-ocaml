open Expr

exception RuntimeError

let interpreter =
  object (self)
    method evaluate =
      function
      | Literal l -> l
      | Grouping expr -> self#evaluate expr
      | _ -> raise RuntimeError
  end
