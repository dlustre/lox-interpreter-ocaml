open Expr

exception RuntimeError of Token.t * string
exception Todo

let is_truthy = function Num _ | String _ -> true | Bool b -> b | Nil -> false
let is_equal = function Nil, Nil -> true | Nil, _ -> false | a, b -> a = b

let interpreter =
  object (self)
    method evaluate =
      function
      | Literal l -> l
      | Grouping expr -> self#evaluate expr
      | Unary { operator = Token { kind = MINUS; _ } as negation; right } -> (
          match self#evaluate right with
          | Num num -> Num ~-.num
          | _ -> raise (RuntimeError (negation, "Operand must be a number.")))
      | Unary { operator = Token { kind = BANG; _ }; right } ->
          let negated = right |> self#evaluate |> is_truthy |> not in
          Bool negated
      | Binary { left; operator = Token { kind = STAR; _ } as operator; right }
        -> (
          match (self#evaluate left, self#evaluate right) with
          | Num left, Num right -> Num (left *. right)
          | _ -> raise (RuntimeError (operator, "Operands must be a numbers.")))
      | Binary { left; operator = Token { kind = SLASH; _ } as operator; right }
        -> (
          match (self#evaluate left, self#evaluate right) with
          | Num left, Num right -> Num (left /. right)
          | _ -> raise (RuntimeError (operator, "Operands must be a numbers.")))
      | _ -> raise Todo
  end
