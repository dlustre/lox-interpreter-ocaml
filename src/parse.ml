open Token
open Expr

exception ParseError of string

let parser t =
  object (self)
    val mutable tokens : token list = t
    val mutable previous = None
    val mutable current = 0

    method primary =
      match tokens with
      | Token { kind = FALSE; _ } :: [] -> Literal (Bool false)
      | Token { kind = TRUE; _ } :: [] -> Literal (Bool true)
      | Token { kind = NIL; _ } :: [] -> Literal Nil
      | TokenWithLiteral { kind = NUMBER | STRING; literal; _ } :: [] ->
          Literal (NumOrString literal)
      | (Token { kind = IDENTIFIER; _ } as token) :: [] -> Variable token
      (* | Token { kind = LEFT_PAREN; _ } -> Literal  *)
      | _ -> raise (ParseError "Expect expression.")

    method expression = self#primary

    method to_expr =
      match self#expression with
      | exception ParseError msg ->
          print_endline msg;
          None
      | expr -> Some expr
  end
