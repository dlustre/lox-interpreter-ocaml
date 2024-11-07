open Token
open Expr
open Stmt

exception ParseError of Token.t * string
exception Unreachable

let parser t =
  object (self)
    val mutable tokens = t
    val mutable current = 0

    method advance =
      match tokens with
      | token :: rest ->
          tokens <- rest;
          current <- current + 1;
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

    (** Evaluate the [reduce] value on each token that matches any of [token_kinds], using a [predicate]. *)

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

    method unary =
      match tokens with
      | operator :: _ when self#match_any [ BANG; MINUS ] ->
          let right = self#unary in
          Unary { operator; right }
      | _ -> self#primary

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

    method assignment =
      let expr = self#equality in

      match tokens with
      | (Token _ as equals) :: _ when self#match_any [ EQUAL ] -> (
          let value = self#assignment in
          match expr with
          | Variable name -> Assign { name; value }
          | _ -> raise @@ ParseError (equals, "Invalid assignment target."))
      | _ -> expr

    method expression = self#assignment
    method to_expr = self#expression

    method statement =
      match tokens with
      | _ when self#match_any [ PRINT ] ->
          let value = self#expression in
          let _ = self#consume_semicolon "Expect ';' after value." in
          Print value
      | _ ->
          let expr = self#expression in
          let _ = self#consume_semicolon "Expect ';' after expression." in
          Expression expr

    method declaration =
      match tokens with
      | _ when self#match_any [ VAR ] -> (
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
              Var name)
      | _ -> self#statement

    method to_stmts stmts =
      match tokens with
      | Token { kind = EOF; _ } :: [] -> List.rev stmts
      | _ ->
          self#to_stmts
            (match self#declaration with
            | exception ParseError (token, msg) ->
                Error.of_error token msg;
                self#synchronize;
                stmts
            | stmt -> stmt :: stmts)
  end
