open Expr

exception RuntimeError

let interpreter =
  object (_self)
    method evaluate = function Literal _ as l -> l | _ -> raise RuntimeError
  end
