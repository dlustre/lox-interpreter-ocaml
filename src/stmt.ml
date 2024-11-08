open Expr

type stmt =
  | Print of expr
  | Expression of expr
  | Var of Token.t
  | VarWithInit of Token.t * expr
  | Block of stmt list
  | If of { condition : expr; then_branch : stmt; else_branch : stmt option }
  | While of { condition : expr; body : stmt }
