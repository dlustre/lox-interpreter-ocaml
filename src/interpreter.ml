open Shared.Expr
open Shared
open Shared.Stmt
open Env
open Error

let do_log = false

let is_truthy = function
  | Callable _ | Num _ | String _ -> true
  | Bool b -> b
  | Nil -> false

let is_equal = function Nil, Nil -> true | Nil, _ -> false | a, b -> a = b

let binary = function
  | _, fn, Num left, Num right -> fn left right
  | operator, _, _, _ ->
      raise @@ RuntimeError (operator, "Operands must be numbers.")

let globals : literal env =
  let g : literal env = new env None literal_to_string (Env.get_counter ()) in
  g#define "clock" (Callable Native.clock);
  g

let interpreter =
  object (self)
    val mutable env = globals
    val globals = globals
    method get_globals = globals

    method evaluate =
      function
      | Literal l -> l
      | Grouping expr -> self#evaluate expr
      | Variable (_ as name) -> env#get name
      | Call { callee; paren; args } ->
          let f =
            match self#evaluate callee with
            | Callable c -> c
            | _ ->
                raise
                @@ RuntimeError (paren, "Can only call functions and classes.")
          in
          let args = List.map self#evaluate args in

          if List.length args <> f#arity then
            raise
            @@ RuntimeError
                 ( paren,
                   Printf.sprintf "Expected %d arguments but got %d." f#arity
                   @@ List.length args );

          f#call self args
      | Assign { name; value } ->
          let value = self#evaluate value in
          env#assign name value;
          value
      | Unary { operator = { kind = MINUS; _ } as negation; right } -> (
          match self#evaluate right with
          | Num num -> Num ~-.num
          | _ -> raise @@ RuntimeError (negation, "Operand must be a number."))
      | Unary { operator = { kind = BANG; _ }; right } ->
          let logic_negated = right |> self#evaluate |> is_truthy |> not in
          Bool logic_negated
      | Binary { left; operator = { kind = STAR; _ } as op; right } ->
          Num (binary (op, ( *. ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = { kind = SLASH; _ } as op; right } ->
          Num (binary (op, ( /. ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = { kind = PLUS; _ } as op; right } -> (
          match (self#evaluate left, self#evaluate right) with
          | Num left, Num right -> Num (left +. right)
          | String left, String right -> String (left ^ right)
          | _ ->
              raise
              @@ RuntimeError
                   (op, "Operands must be two numbers or two strings."))
      | Binary { left; operator = { kind = MINUS; _ } as op; right } ->
          Num (binary (op, ( -. ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = { kind = GREATER; _ } as op; right } ->
          Bool (binary (op, ( > ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = { kind = GREATER_EQUAL; _ } as op; right } ->
          Bool (binary (op, ( >= ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = { kind = LESS_EQUAL; _ } as op; right } ->
          Bool (binary (op, ( <= ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = { kind = LESS; _ } as op; right } ->
          Bool (binary (op, ( < ), self#evaluate left, self#evaluate right))
      | Binary { left; operator = { kind = EQUAL_EQUAL; _ }; right } ->
          Bool (self#evaluate left = self#evaluate right)
      | Binary { left; operator = { kind = BANG_EQUAL; _ }; right } ->
          Bool (self#evaluate left <> self#evaluate right)
      | Logical { left; operator = { kind = OR; _ }; right } ->
          let left = self#evaluate left in
          if is_truthy left then left else self#evaluate right
      | Logical { left; operator = { kind = AND; _ }; right } ->
          let left = self#evaluate left in
          if not @@ is_truthy left then left else self#evaluate right
      | _ -> raise Todo

    method execute =
      function
      | Print expr ->
          expr |> self#evaluate |> literal_to_string |> print_endline
      | Var { lexeme; _ } -> env#define lexeme Nil
      | VarWithInit ({ lexeme; _ }, init) ->
          env#define lexeme (self#evaluate init)
      | Expression expr ->
          let _ = self#evaluate expr in
          ()
      | Block stmts ->
          self#execute_block stmts
          @@ new env (Some env) literal_to_string (Env.get_counter ())
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
      | Function { name = { lexeme; _ }; _ } as fn ->
          env#define lexeme @@ Callable (LoxFunction.make fn env)
      | Return { value = None; _ } -> raise @@ Return Nil
      | Return { value = Some value; _ } ->
          raise @@ Return (self#evaluate value)

    method execute_block stmts block_env =
      if do_log then (
        print_endline "block env:";
        block_env#print_recursive);
      let prev = env in
      Fun.protect
        ~finally:(fun _ ->
          if do_log then print_endline "recovering to prev scope shown below:";
          prev#print;
          env <- prev)
        (fun _ ->
          env <- block_env;
          self#interpret_stmts stmts)

    method interpret_stmts = List.iter (fun stmt -> self#execute stmt)
  end
