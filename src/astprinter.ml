open Expr

let print = function
  | Literal literal -> literal |> expr_literal_to_string |> print_endline
  | _ -> print_endline "unknown"
