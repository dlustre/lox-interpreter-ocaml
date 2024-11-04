open Token

type expr_literal = NumOrString of literal | Bool of bool | Nil

type expr =
  | Assign of { name : token; value : expr }
  | Binary of { left : expr; operator : token; right : expr }
  | Grouping of expr
  | Literal of expr_literal
  | Logical of { left : expr; operator : token; right : expr }
  | Unary of { operator : token; right : expr }
  | Variable of token

let expr_literal_to_string = function
  | NumOrString literal -> literal_to_string literal
  | Bool bool -> Printf.sprintf "%B" bool
  | Nil -> "nil"
