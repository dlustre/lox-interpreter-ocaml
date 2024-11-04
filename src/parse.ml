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

    (** Evaluate the [reduce] value on each token that matches any of [token_kinds], using a [predicate]. *)

    method reduce_all_matches token_kinds predicate reduce =
      match tokens with
      | token :: _ when self#match_any token_kinds ->
          self#reduce_all_matches token_kinds predicate (predicate token reduce)
      | _ -> reduce

    method check token_kind =
      match tokens with
      | Token { kind = EOF; _ } :: _ -> false
      | Token { kind; _ } :: _ | TokenWithLiteral { kind; _ } :: _ ->
          kind = token_kind
      | _ -> raise (ParseError "Unexpectedly exhausted tokens.")

    method match_any token_kinds =
      if List.exists (function kind -> self#check kind) token_kinds then
        let _ = self#advance in
        true
      else false

    method primary =
      match tokens with
      | _ when self#match_any [ FALSE ] -> Literal (Bool false)
      | _ when self#match_any [ TRUE ] -> Literal (Bool true)
      | _ when self#match_any [ NIL ] -> Literal Nil
      | TokenWithLiteral { literal; _ } :: _
        when self#match_any [ NUMBER; STRING ] ->
          Literal (NumOrString literal)
      | variable :: _ when self#match_any [ IDENTIFIER ] -> Variable variable
      | _ when self#match_any [ LEFT_PAREN ] ->
          let expr = self#expression in
          let _ = self#consume RIGHT_PAREN "Expect ')' after expression." in
          Grouping expr
      | _ -> raise (ParseError "Expect expression.")

    method unary =
      match tokens with
      | operator :: _ when self#match_any [ BANG; MINUS ] ->
          let right = self#unary in
          Unary { operator; right }
      | _ -> self#primary

    method factor =
      self#reduce_all_matches [ SLASH; STAR ]
        (fun operator reduce ->
          Binary { left = reduce; operator; right = self#unary })
        self#unary

    method term =
      self#reduce_all_matches [ MINUS; PLUS ]
        (fun operator reduce ->
          Binary { left = reduce; operator; right = self#factor })
        self#factor

    method expression = self#term

    method to_expr =
      match self#expression with
      | exception ParseError msg ->
          print_endline msg;
          None
      | expr -> Some expr
  end
