open Expr

exception RuntimeError of Token.t * string
exception Todo

let is_truthy = function NumOrString _ -> true | Bool b -> b | Nil -> false
let is_equal = function Nil, Nil -> true | Nil, _ -> false | a, b -> a = b

let interpreter =
  object (self)
    method evaluate =
      function
      | Literal l -> l
      | Grouping expr -> self#evaluate expr
      | Unary { operator = Token { kind = MINUS; _ } as negation; right } -> (
          match self#evaluate right with
          | NumOrString (Token.NumberLiteral num) ->
              NumOrString (Token.NumberLiteral ~-.num)
          | _ -> raise (RuntimeError (negation, "Operand must be a number.")))
      | Unary { operator = Token { kind = BANG; _ }; right } ->
          let negated = right |> self#evaluate |> is_truthy |> not in
          Bool negated
      | _ -> raise Todo
  end
