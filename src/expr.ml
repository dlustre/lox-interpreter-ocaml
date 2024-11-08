type expr_literal = Num of float | String of string | Bool of bool | Nil

type expr =
  | Assign of { name : Token.t; value : expr }
  | Binary of { left : expr; operator : Token.t; right : expr }
  | Call of { callee : expr; paren : Token.t; args : expr list }
  | Grouping of expr
  | Literal of expr_literal
  | Logical of { left : expr; operator : Token.t; right : expr }
  | Unary of { operator : Token.t; right : expr }
  | Variable of Token.t

let expr_literal_to_string = function
  | Num num -> Token.literal_to_string (NumberLiteral num)
  | String string -> Token.literal_to_string (StringLiteral string)
  | Bool bool -> Printf.sprintf "%B" bool
  | Nil -> "nil"

let rec to_string = function
  | Literal literal -> expr_literal_to_string literal
  | Grouping expr -> parenthesize ("group", [ expr ], "")
  | Unary { operator = Token { lexeme; _ }; right } ->
      parenthesize (lexeme, [ right ], "")
  | Binary { left; operator = Token { lexeme; _ }; right } ->
      parenthesize (lexeme, [ left; right ], "")
  | _ -> "Unknown Expr."

and parenthesize = function
  | name, exprs, "" -> parenthesize (name, exprs, "(" ^ name)
  | _, [], acc -> acc ^ ")"
  | name, expr :: rest, acc ->
      parenthesize (name, rest, acc ^ " " ^ to_string expr)
