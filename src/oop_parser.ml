open Token
open Expr
open Stmt
open Error

exception NilPrevious

class parser t =
  object (self)
    val mutable tokens = t
    val mutable current = 0
    val mutable previous = None

    method advance =
      match tokens with
      | Token { kind = EOF; _ } :: [] -> (
          match previous with None -> raise NilPrevious | Some token -> token)
      | token :: rest ->
          tokens <- rest;
          current <- current + 1;
          previous <- Some token;
          token
      | [] -> raise Unreachable

    method consume token_kind error_msg =
      match tokens with
      | (Token { kind; _ } :: _ | TokenWithLiteral { kind; _ } :: _)
        when kind = token_kind ->
          self#advance
      | token :: _ -> raise @@ ParseError (token, error_msg)
      | [] -> raise Unreachable

    method consume_semicolon = self#consume SEMICOLON

    (** 
    Evaluate the [reduce] value on each subsequent token that matches any of [token_kinds], using a [predicate]. 
    Intended for parsing chainable expressions (e.g. 'x or y or z').
    *)

    method reduce_all_matches token_kinds predicate reduce =
      match tokens with
      | token :: _ when self#match_any token_kinds ->
          self#reduce_all_matches token_kinds predicate (predicate token reduce)
      | _ -> reduce

    method check token_kind =
      match tokens with
      | Token { kind = EOF; _ } :: [] -> false
      | Token { kind; _ } :: _ | TokenWithLiteral { kind; _ } :: _ ->
          kind = token_kind
      | [] -> raise Unreachable

    method match_any token_kinds =
      if List.exists (function kind -> self#check kind) token_kinds then
        let _ = self#advance in
        true
      else false

    method synchronize =
      let _ = self#advance in
      self#do_synchronize

    method do_synchronize =
      match tokens with
      | Token { kind = EOF; _ } :: [] -> ()
      | _ when self#match_any [ SEMICOLON ] -> ()
      | Token
          { kind = CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN; _ }
        :: _ ->
          ()
      | _ -> self#synchronize

    method primary =
      match tokens with
      | _ when self#match_any [ FALSE ] -> Literal (Bool false)
      | _ when self#match_any [ TRUE ] -> Literal (Bool true)
      | _ when self#match_any [ NIL ] -> Literal Nil
      | TokenWithLiteral { literal = NumberLiteral num; _ } :: _
        when self#match_any [ NUMBER ] ->
          Literal (Num num)
      | TokenWithLiteral { literal = StringLiteral string; _ } :: _
        when self#match_any [ STRING ] ->
          Literal (String string)
      | variable :: _ when self#match_any [ IDENTIFIER ] -> Variable variable
      | _ when self#match_any [ LEFT_PAREN ] ->
          let expr = self#expression in
          let _ = self#consume RIGHT_PAREN "Expect ')' after expression." in
          Grouping expr
      | token :: _ -> raise @@ ParseError (token, "Expect expression.")
      | [] -> raise Unreachable

    method finish_call callee =
      let rec consume_args args =
        if List.length args >= 255 then
          Error.of_error (List.hd tokens) "Can't have more than 255 arguments.";
        let args = self#expression :: args in
        if self#match_any [ COMMA ] then consume_args @@ args else List.rev args
      in

      let args =
        if not @@ self#check RIGHT_PAREN then consume_args [] else []
      in

      let paren = self#consume RIGHT_PAREN "Expect ')' after arguments." in
      Call { callee; paren; args }

    method call =
      self#reduce_all_matches [ LEFT_PAREN ]
        (fun _ callee -> self#finish_call callee)
        self#primary

    method unary =
      match tokens with
      | operator :: _ when self#match_any [ BANG; MINUS ] ->
          let right = self#unary in
          Unary { operator; right }
      | _ -> self#call

    method factor =
      self#reduce_all_matches [ SLASH; STAR ]
        (fun operator left -> Binary { left; operator; right = self#unary })
        self#unary

    method term =
      self#reduce_all_matches [ MINUS; PLUS ]
        (fun operator left -> Binary { left; operator; right = self#factor })
        self#factor

    method comparison =
      self#reduce_all_matches
        [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ]
        (fun operator left -> Binary { left; operator; right = self#term })
        self#term

    method equality =
      self#reduce_all_matches
        [ BANG_EQUAL; EQUAL_EQUAL ]
        (fun operator left ->
          Binary { left; operator; right = self#comparison })
        self#comparison

    method logic_and =
      self#reduce_all_matches [ AND ]
        (fun operator left -> Logical { left; operator; right = self#equality })
        self#equality

    method logic_or =
      self#reduce_all_matches [ OR ]
        (fun operator left ->
          Logical { left; operator; right = self#logic_and })
        self#logic_and

    method assignment =
      let expr = self#logic_or in

      match tokens with
      | (Token _ as equals) :: _ when self#match_any [ EQUAL ] -> (
          let value = self#assignment in
          match expr with
          | Variable name -> Assign { name; value }
          | _ -> raise @@ ParseError (equals, "Invalid assignment target."))
      | _ -> expr

    method expression = self#assignment
    method to_expr = self#expression

    method expression_stmt =
      let expr = self#expression in
      let _ = self#consume_semicolon "Expect ';' after expression." in
      Expression expr

    method for_stmt =
      let _ = self#consume LEFT_PAREN "Expect '(' after 'for'." in
      let init =
        match tokens with
        | _ when self#match_any [ SEMICOLON ] -> None
        | _ when self#match_any [ VAR ] -> Some self#var_declaration
        | _ -> Some self#expression_stmt
      in
      let condition =
        match tokens with
        | Token { kind = SEMICOLON; _ } :: _ -> Literal (Bool true)
        | _ -> self#expression
      in
      let _ = self#consume_semicolon "Expect ';' after loop condition." in
      let increment =
        match tokens with
        | Token { kind = RIGHT_PAREN; _ } :: _ -> None
        | _ -> Some (Expression self#expression)
      in
      let _ = self#consume RIGHT_PAREN "Expect ')' after for clauses." in
      let body = self#statement in
      match (init, condition, increment) with
      | None, condition, None -> While { condition; body }
      | Some init, condition, None -> Block [ init; While { condition; body } ]
      | None, condition, Some increment ->
          While { condition; body = Block [ body; increment ] }
      | Some init, condition, Some increment ->
          Block [ init; While { condition; body = Block [ body; increment ] } ]

    method statement =
      match tokens with
      | _ when self#match_any [ LEFT_BRACE ] -> Block (self#block [])
      | _ when self#match_any [ PRINT ] ->
          let value = self#expression in
          let _ = self#consume_semicolon "Expect ';' after value." in
          Print value
      | _ when self#match_any [ IF ] -> (
          let _ = self#consume LEFT_PAREN "Expect '(' after 'if'." in
          let condition = self#expression in
          let _ = self#consume RIGHT_PAREN "Expect ')' after if condition." in
          let then_branch = self#statement in
          match tokens with
          | _ when self#match_any [ ELSE ] ->
              let else_branch = Some self#statement in
              If { condition; then_branch; else_branch }
          | _ -> If { condition; then_branch; else_branch = None })
      | _ when self#match_any [ WHILE ] ->
          let _ = self#consume LEFT_PAREN "Expect '(' after 'while'." in
          let condition = self#expression in
          let _ =
            self#consume RIGHT_PAREN "Expect ')' after while condition."
          in
          let body = self#statement in
          While { condition; body }
      | _ when self#match_any [ FOR ] -> self#for_stmt
      | _ -> self#expression_stmt

    method var_declaration =
      let name = self#consume IDENTIFIER "Expect variable name." in
      match tokens with
      | _ when self#match_any [ EQUAL ] ->
          let init = self#expression in
          let _ =
            self#consume_semicolon "Expect ';' after variable declaration."
          in
          VarWithInit (name, init)
      | _ ->
          let _ =
            self#consume_semicolon "Expect ';' after variable declaration."
          in
          Var name

    method declaration =
      try
        match tokens with
        | _ when self#match_any [ VAR ] -> Ok self#var_declaration
        | _ -> Ok self#statement
      with ParseError (token, msg) ->
        Error.of_error token msg;
        self#synchronize;
        Error ()

    method block stmts =
      match tokens with
      | Token { kind = RIGHT_BRACE; _ } :: _ | Token { kind = EOF; _ } :: [] ->
          let _ = self#consume RIGHT_BRACE "Expect '}' after block." in
          List.rev stmts
      | _ ->
          self#block
            (match self#declaration with
            | Error _ -> stmts
            | Ok stmt -> stmt :: stmts)

    method to_stmts stmts =
      match tokens with
      | Token { kind = EOF; _ } :: [] -> List.rev stmts
      | _ ->
          self#to_stmts
            (match self#declaration with
            | Error _ -> stmts
            | Ok stmt -> stmt :: stmts)
  end
