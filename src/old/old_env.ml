open Shared
open Token
open Error
module StringMap = Map.Make (String)

type env = { enclosing : env option; mutable values : expr_literal StringMap.t }

let rec get name env =
  let lexeme =
    match name with
    | Token { lexeme; _ } | TokenWithLiteral { lexeme; _ } -> lexeme
  in

  match env with
  | { enclosing = None; values } -> (
      match StringMap.find_opt lexeme values with
      | None ->
          raise
          @@ RuntimeError
               (name, Printf.sprintf "Undefined variable '%s'." lexeme)
      | Some value -> value)
  | { enclosing = Some enclosing; values } -> (
      match StringMap.find_opt lexeme values with
      | None -> get name enclosing
      | Some value -> value)

let rec assign name value env =
  let lexeme =
    match name with
    | Token.Token { lexeme; _ } | Token.TokenWithLiteral { lexeme; _ } -> lexeme
  in
  match env with
  | { enclosing = None; values } -> (
      match StringMap.find_opt lexeme values with
      | None ->
          raise
          @@ RuntimeError
               (name, Printf.sprintf "Undefined variable '%s'." lexeme)
      | Some _ -> env.values <- StringMap.add lexeme value values)
  | { enclosing = Some enclosing; values } -> (
      match StringMap.find_opt lexeme values with
      | None -> assign name value enclosing
      | Some _ -> env.values <- StringMap.add lexeme value values)

let define name value env =
  match env with
  | { enclosing = None; values } ->
      env.values <- StringMap.add name value values
  | { enclosing = Some _; values } ->
      env.values <- StringMap.add name value values

let rec print_vals = function
  | { enclosing = None; values }, n ->
      StringMap.iter
        (fun key value ->
          Printf.printf "[%d]: Key: %s, Value: %s\n" n key
            (expr_literal_to_string value))
        values
  | { enclosing = Some enclosing; values }, n ->
      StringMap.iter
        (fun key value ->
          Printf.printf "[%d]: Key: %s, Value: %s\n" n key
            (expr_literal_to_string value))
        values;
      print_vals (enclosing, n + 1)
