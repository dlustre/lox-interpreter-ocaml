open Expr

type stmt =
  | Print of expr
  | Expression of expr
  | Var of Token.t
  | VarWithInit of Token.t * expr
  | Block of stmt list
