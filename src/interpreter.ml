open Expr

exception RuntimeError of Token.t * string
exception Todo

let is_truthy = function Num _ | String _ -> true | Bool b -> b | Nil -> false
let is_equal = function Nil, Nil -> true | Nil, _ -> false | a, b -> a = b

let binary_math = function
  | _, fn, Num left, Num right -> Num (fn left right)
  | operator, _, _, _ ->
      raise (RuntimeError (operator, "Operands must be numbers."))

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
        ->
          binary_math (operator, ( *. ), self#evaluate left, self#evaluate right)
      | Binary { left; operator = Token { kind = SLASH; _ } as operator; right }
        ->
          binary_math (operator, ( /. ), self#evaluate left, self#evaluate right)
      | Binary { left; operator = Token { kind = PLUS; _ } as operator; right }
        -> (
          match (self#evaluate left, self#evaluate right) with
          | Num left, Num right -> Num (left +. right)
          | String left, String right -> String (left ^ right)
          | _ ->
              raise
                (RuntimeError
                   (operator, "Operands must be two numbers or two strings.")))
      | Binary { left; operator = Token { kind = MINUS; _ } as operator; right }
        ->
          binary_math (operator, ( -. ), self#evaluate left, self#evaluate right)
      | _ -> raise Todo
  end
