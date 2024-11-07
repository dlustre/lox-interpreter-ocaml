open Expr
open Token
open Error
module StringMap = Map.Make (String)

type env =
  | GlobalEnv of expr_literal StringMap.t
  | BlockEnv of env * expr_literal StringMap.t

let rec get name env =
  let lexeme =
    match name with
    | Token { lexeme; _ } | TokenWithLiteral { lexeme; _ } -> lexeme
  in

  match env with
  | GlobalEnv env -> (
      match StringMap.find_opt lexeme env with
      | None ->
          raise
          @@ RuntimeError
               (name, Printf.sprintf "Undefined variable '%s'." lexeme)
      | Some value -> value)
  | BlockEnv (enclosing, env) -> (
      match StringMap.find_opt lexeme env with
      | None -> get name enclosing
      | Some value -> value)

let rec assign name value env =
  let lexeme =
    match name with
    | Token.Token { lexeme; _ } | Token.TokenWithLiteral { lexeme; _ } -> lexeme
  in
  match env with
  | GlobalEnv env -> (
      match StringMap.find_opt lexeme env with
      | None ->
          raise
          @@ RuntimeError
               (name, Printf.sprintf "Undefined variable '%s'." lexeme)
      | Some _ -> GlobalEnv (StringMap.add lexeme value env))
  | BlockEnv (enclosing, env) -> (
      match StringMap.find_opt lexeme env with
      | None -> assign name value enclosing
      | Some _ -> BlockEnv (enclosing, StringMap.add lexeme value env))

let define name value env =
  match env with
  | GlobalEnv env -> GlobalEnv (StringMap.add name value env)
  | BlockEnv (enclosing, env) ->
      BlockEnv (enclosing, StringMap.add name value env)
