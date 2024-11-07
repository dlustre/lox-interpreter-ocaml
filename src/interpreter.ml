open Expr
open Stmt

exception RuntimeError of Token.t * string
exception Todo

let is_truthy = function Num _ | String _ -> true | Bool b -> b | Nil -> false
let is_equal = function Nil, Nil -> true | Nil, _ -> false | a, b -> a = b

let binary = function
  | _, fn, Num left, Num right -> fn left right
  | operator, _, _, _ ->
      raise (RuntimeError (operator, "Operands must be numbers."))

module Env = Map.Make (String)

let interpreter =
  object (self)
    val mutable env : expr_literal Env.t = Env.empty

    method evaluate =
      function
      | Literal l -> l
      | Grouping expr -> self#evaluate expr
      | Variable (Token { lexeme; _ } as name) -> (
          match Env.find lexeme env with
          | exception Not_found ->
              raise
                (RuntimeError
                   (name, Printf.sprintf "Undefined variable '%s'." lexeme))
          | value -> value)
      | Unary { operator = Token { kind = MINUS; _ } as negation; right } -> (
          match self#evaluate right with
          | Num num -> Num ~-.num
          | _ -> raise (RuntimeError (negation, "Operand must be a number.")))
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
                (RuntimeError
                   (op, "Operands must be two numbers or two strings.")))
      | Binary { left; operator = Token { kind = MINUS; _ } as op; right } ->
          Num (binary (op, ( -. ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = GREATER; _ } as op; right } ->
          Bool (binary (op, ( > ), self#evaluate left, self#evaluate right))
      | Binary
          { left; operator = Token { kind = GREATER_EQUAL; _ } as op; right } ->
          Bool (binary (op, ( >= ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = LESS_EQUAL; _ } as op; right }
        ->
          Bool (binary (op, ( < ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = LESS; _ } as op; right } ->
          Bool (binary (op, ( <= ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = Token { kind = EQUAL_EQUAL; _ }; right } ->
          Bool (self#evaluate left = self#evaluate right)
      | Binary { left; operator = Token { kind = BANG_EQUAL; _ }; right } ->
          Bool (self#evaluate left <> self#evaluate right)
      | _ -> raise Todo

    method execute =
      function
      | Print expr ->
          expr |> self#evaluate |> expr_literal_to_string |> print_endline
      | Var (Token { lexeme; _ }) -> env <- Env.add lexeme Nil env
      | VarWithInit (Token { lexeme; _ }, init) ->
          env <- Env.add lexeme (self#evaluate init) env
      | Expression expr ->
          let _ = self#evaluate expr in
          ()
      | _ -> raise Todo

    method interpret_stmts =
      function
      | [] -> ()
      | stmt :: rest ->
          self#execute stmt;
          self#interpret_stmts rest
  end
