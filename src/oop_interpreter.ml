open Expr
open Stmt
open Env
open Error

let is_truthy = function Num _ | String _ -> true | Bool b -> b | Nil -> false
let is_equal = function Nil, Nil -> true | Nil, _ -> false | a, b -> a = b

let binary = function
  | _, fn, Num left, Num right -> fn left right
  | operator, _, _, _ ->
      raise @@ RuntimeError (operator, "Operands must be numbers.")

let globals = { enclosing = None; values = StringMap.empty }

class interpreter =
  object (self)
    val mutable env = globals

    method evaluate =
      function
      | Literal l -> l
      | Grouping expr -> self#evaluate expr
      | Variable (Token _ as name) -> get name env
      | Assign { name; value } ->
          let value = self#evaluate value in
          assign name value env;
          value
      | Unary { operator = Token { kind = MINUS; _ } as negation; right } -> (
          match self#evaluate right with
          | Num num -> Num ~-.num
          | _ -> raise @@ RuntimeError (negation, "Operand must be a number."))
      | Unary { operator = Token { kind = BANG; _ }; right } ->
          let logic_negated = right |> self#evaluate |> is_truthy |> not in
          Bool logic_negated
      | Binary { left; operator = Token { kind = STAR; _ } as op; right } ->
          Num (binary (op, ( *. ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = SLASH; _ } as op; right } ->
          Num (binary (op, ( /. ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = PLUS; _ } as op; right } -> (
          match (self#evaluate left, self#evaluate right) with
          | Num left, Num right -> Num (left +. right)
          | String left, String right -> String (left ^ right)
          | _ ->
              raise
              @@ RuntimeError
                   (op, "Operands must be two numbers or two strings."))
      | Binary { left; operator = Token { kind = MINUS; _ } as op; right } ->
          Num (binary (op, ( -. ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = GREATER; _ } as op; right } ->
          Bool (binary (op, ( > ), self#evaluate left, self#evaluate right))
      | Binary
          { left; operator = Token { kind = GREATER_EQUAL; _ } as op; right } ->
          Bool (binary (op, ( >= ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = LESS_EQUAL; _ } as op; right }
        ->
          Bool (binary (op, ( <= ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = LESS; _ } as op; right } ->
          Bool (binary (op, ( < ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = EQUAL_EQUAL; _ }; right } ->
          Bool (self#evaluate left = self#evaluate right)
      | Binary { left; operator = Token { kind = BANG_EQUAL; _ }; right } ->
          Bool (self#evaluate left <> self#evaluate right)
      | Logical { left; operator = Token { kind = OR; _ }; right } ->
          let left = self#evaluate left in
          if is_truthy left then left else self#evaluate right
      | Logical { left; operator = Token { kind = AND; _ }; right } ->
          let left = self#evaluate left in
          if not @@ is_truthy left then left else self#evaluate right
      | _ -> raise Todo

    method execute =
      function
      | Print expr ->
          expr |> self#evaluate |> expr_literal_to_string |> print_endline
      | Var (Token { lexeme; _ }) -> define lexeme Nil env
      | VarWithInit (Token { lexeme; _ }, init) ->
          define lexeme (self#evaluate init) env
      | Expression expr ->
          let _ = self#evaluate expr in
          ()
      | Block stmts ->
          let prev = env in
          Fun.protect ~finally:(fun _ -> env <- prev) @@ fun _ ->
          env <- { enclosing = Some env; values = StringMap.empty };
          self#interpret_stmts stmts
      | If { condition; then_branch; else_branch = None } ->
          if condition |> self#evaluate |> is_truthy then
            self#execute then_branch
      | If { condition; then_branch; else_branch = Some else_branch } ->
          if condition |> self#evaluate |> is_truthy then
            self#execute then_branch
          else self#execute else_branch
      | While { condition; body } ->
          let while_cond = ref @@ self#evaluate condition in
          while is_truthy !while_cond do
            self#execute body;
            while_cond := self#evaluate condition
          done
      | _ -> raise Todo

    method interpret_stmts = List.iter (fun stmt -> self#execute stmt)
  end
