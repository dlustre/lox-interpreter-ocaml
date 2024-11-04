open Token
open Expr

exception ParseError of string

let parser t =
  object (self)
    val mutable tokens = t
    val mutable current = 0

    method advance =
      match tokens with
      | token :: rest ->
          tokens <- rest;
          token
      | _ -> raise (ParseError "Unexpectedly exhausted tokens.")

    method consume token_kind error_msg =
      match tokens with
      | (Token { kind; _ } :: _ | TokenWithLiteral { kind; _ } :: _)
        when kind = token_kind ->
          self#advance
      | _ -> raise (ParseError error_msg)

    method primary =
      match self#advance with
      | Token { kind = FALSE; _ } -> Literal (Bool false)
      | Token { kind = TRUE; _ } -> Literal (Bool true)
      | Token { kind = NIL; _ } -> Literal Nil
      | TokenWithLiteral { kind = NUMBER | STRING; literal; _ } ->
          Literal (NumOrString literal)
      | Token { kind = IDENTIFIER; _ } as token -> Variable token
      | Token { kind = LEFT_PAREN; _ } ->
          let expr = self#expression in
          let _ = self#consume RIGHT_PAREN "Expect ')' after expression." in
          Grouping expr
      | _ -> raise (ParseError "Expect expression.")

    method expression = self#primary

    method to_expr =
      match self#expression with
      | exception ParseError msg ->
          print_endline msg;
          None
      | expr -> Some expr
  end
