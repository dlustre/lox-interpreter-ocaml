open Token

type expr_literal = NumOrString of literal | Bool of bool | Nil

type expr =
  | Assign of { name : Token.t; value : expr }
  | Binary of { left : expr; operator : Token.t; right : expr }
  | Grouping of expr
  | Literal of expr_literal
  | Logical of { left : expr; operator : Token.t; right : expr }
  | Unary of { operator : Token.t; right : expr }
  | Variable of Token.t

let expr_literal_to_string = function
  | NumOrString literal -> literal_to_string literal
  | Bool bool -> Printf.sprintf "%B" bool
  | Nil -> "nil"

let rec to_string = function
  | Literal literal -> literal |> expr_literal_to_string
  | Grouping expr -> "(group " ^ to_string expr ^ ")"
  | _ -> "unknown expr, can't print"
