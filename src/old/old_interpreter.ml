open Expr
open Env
open Error

let is_truthy = function Num _ | String _ -> true | Bool b -> b | Nil -> false
let is_equal = function Nil, Nil -> true | Nil, _ -> false | a, b -> a = b

let binary = function
  | _, fn, Num left, Num right -> fn left right
  | operator, _, _, _ ->
      raise @@ RuntimeError (operator, "Operands must be numbers.")

type t = { globals : env; mutable env : env }

let globals = { enclosing = None; values = StringMap.empty }
let ctx = { globals; env = globals }

let rec evaluate = function
  | Literal l -> l
  | Grouping expr -> evaluate expr
  | Variable (Token _ as name) -> get name ctx.env
  | Assign { name; value } ->
      let value = evaluate value in
      assign name value ctx.env;
      value
  | Unary { operator = Token { kind = MINUS; _ } as negation; right } -> (
      match evaluate right with
      | Num num -> Num ~-.num
      | _ -> raise @@ RuntimeError (negation, "Operand must be a number."))
  | Unary { operator = Token { kind = BANG; _ }; right } ->
      let logic_negated = right |> evaluate |> is_truthy |> not in
      Bool logic_negated
  | Binary { left; operator = Token { kind = STAR; _ } as op; right } ->
      Num (binary (op, ( *. ), evaluate left, evaluate right))
  | Binary { left; operator = Token { kind = SLASH; _ } as op; right } ->
      Num (binary (op, ( /. ), evaluate left, evaluate right))
  | Binary { left; operator = Token { kind = PLUS; _ } as op; right } -> (
      match (evaluate left, evaluate right) with
      | Num left, Num right -> Num (left +. right)
      | String left, String right -> String (left ^ right)
      | _ ->
          raise
          @@ RuntimeError (op, "Operands must be two numbers or two strings."))
  | Binary { left; operator = Token { kind = MINUS; _ } as op; right } ->
      Num (binary (op, ( -. ), evaluate left, evaluate right))
  | Binary { left; operator = Token { kind = GREATER; _ } as op; right } ->
      Bool (binary (op, ( > ), evaluate left, evaluate right))
  | Binary { left; operator = Token { kind = GREATER_EQUAL; _ } as op; right }
    ->
      Bool (binary (op, ( >= ), evaluate left, evaluate right))
  | Binary { left; operator = Token { kind = LESS_EQUAL; _ } as op; right } ->
      Bool (binary (op, ( <= ), evaluate left, evaluate right))
  | Binary { left; operator = Token { kind = LESS; _ } as op; right } ->
      Bool (binary (op, ( < ), evaluate left, evaluate right))
  | Binary { left; operator = Token { kind = EQUAL_EQUAL; _ }; right } ->
      Bool (evaluate left = evaluate right)
  | Binary { left; operator = Token { kind = BANG_EQUAL; _ }; right } ->
      Bool (evaluate left <> evaluate right)
  | Logical { left; operator = Token { kind = OR; _ }; right } ->
      let left = evaluate left in
      if is_truthy left then left else evaluate right
  | Logical { left; operator = Token { kind = AND; _ }; right } ->
      let left = evaluate left in
      if not @@ is_truthy left then left else evaluate right
  | _ -> raise Todo

and execute = function
  | Print expr -> evaluate expr |> expr_literal_to_string |> print_endline
  | Var (Token { lexeme; _ }) -> define lexeme Nil ctx.env
  | VarWithInit (Token { lexeme; _ }, init) ->
      define lexeme (evaluate init) ctx.env
  | Expression expr ->
      let _ = evaluate expr in
      ()
  | Block stmts ->
      let prev = ctx.env in
      Fun.protect ~finally:(fun _ -> ctx.env <- prev) @@ fun _ ->
      ctx.env <- { enclosing = Some ctx.env; values = StringMap.empty };
      List.iter (fun stmt -> execute stmt) stmts
  | If { condition; then_branch; else_branch = None } ->
      if condition |> evaluate |> is_truthy then execute then_branch
  | If { condition; then_branch; else_branch = Some else_branch } ->
      if condition |> evaluate |> is_truthy then execute then_branch
      else execute else_branch
  | While { condition; body } ->
      let while_cond = ref @@ evaluate condition in
      while is_truthy !while_cond do
        execute body;
        while_cond := evaluate condition
      done
  | _ -> raise Todo

let interpret_stmts = List.iter (fun stmt -> execute stmt)
