open Token
open Expr
open Error

exception NilPrevious

type ctx = {
  mutable tokens : Token.t list;
  mutable current : int;
  mutable previous : Token.t option;
}

module type Parser = sig
  val ctx : ctx
  val advance : unit -> Token.t
  val consume : Token.kind -> string -> Token.t
  val consume_semicolon : string -> Token.t
  val check : Token.kind -> bool
  val match_any : Token.kind list -> bool

  val reduce_all_matches :
    Token.kind list -> (Token.t -> expr -> expr) -> expr -> expr

  val synchronize : unit -> unit

  (* Expr functions. *)
  val to_expr : unit -> expr
  val expression : unit -> expr
  val assignment : unit -> expr
  val logic_or : unit -> expr
  val logic_and : unit -> expr
  val equality : unit -> expr
  val comparison : unit -> expr
  val term : unit -> expr
  val factor : unit -> expr
  val call : unit -> expr
  val finish_call : expr -> expr
  val unary : unit -> expr
  val primary : unit -> expr

  (* Stmt functions. *)
  val to_stmts : stmt list -> stmt list
  val statement : unit -> stmt
  val expression_stmt : unit -> stmt
  val for_stmt : unit -> stmt
  val var_declaration : unit -> stmt
  val declaration : unit -> (stmt, unit) result
  val block : stmt list -> stmt list
end

module Make (Deps : sig
  val tokens : Token.t list
end) : Parser = struct
  let ctx = { tokens = Deps.tokens; current = 0; previous = None }

  let advance () =
    match ctx.tokens with
    | Token { kind = EOF; _ } :: [] -> (
        match ctx.previous with
        | None -> raise NilPrevious
        | Some token -> token)
    | token :: rest ->
        ctx.tokens <- rest;
        ctx.current <- ctx.current + 1;
        ctx.previous <- Some token;
        token
    | [] -> raise Unreachable

  let consume token_kind error_msg =
    match ctx.tokens with
    | (Token { kind; _ } :: _ | TokenWithLiteral { kind; _ } :: _)
      when kind = token_kind ->
        advance ()
    | token :: _ -> raise @@ ParseError (token, error_msg)
    | [] -> raise Unreachable

  let consume_semicolon = consume SEMICOLON

  let check token_kind =
    match ctx.tokens with
    | Token { kind = EOF; _ } :: [] -> false
    | Token { kind; _ } :: _ | TokenWithLiteral { kind; _ } :: _ ->
        kind = token_kind
    | [] -> raise Unreachable

  let match_any token_kinds =
    if List.exists (function kind -> check kind) token_kinds then
      let _ = advance () in
      true
    else false

  (**
  Evaluate the [reduce] value on each subsequent token that matches any of [token_kinds], using a [predicate].
  Intended for parsing chainable expressions (e.g. 'x or y or z').
  *)

  let rec reduce_all_matches token_kinds predicate reduce =
    match ctx.tokens with
    | token :: _ when match_any token_kinds ->
        reduce_all_matches token_kinds predicate (predicate token reduce)
    | _ -> reduce

  let rec synchronize () =
    let _ = advance () in
    let do_synchronize =
      match ctx.tokens with
      | Token { kind = EOF; _ } :: [] -> ()
      | _ when match_any [ SEMICOLON ] -> ()
      | Token
          { kind = CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN; _ }
        :: _ ->
          ()
      | _ -> synchronize ()
    in
    do_synchronize

  let rec to_expr () = expression ()
  and expression () = assignment ()

  and assignment () =
    let expr = logic_or () in

    match ctx.tokens with
    | (Token _ as equals) :: _ when match_any [ EQUAL ] -> (
        let value = assignment () in
        match expr with
        | Variable name -> Assign { name; value }
        | _ -> raise @@ ParseError (equals, "Invalid assignment target."))
    | _ -> expr

  and logic_or () =
    reduce_all_matches [ OR ]
      (fun operator left -> Logical { left; operator; right = logic_and () })
      (logic_and ())

  and logic_and () =
    reduce_all_matches [ AND ]
      (fun operator left -> Logical { left; operator; right = equality () })
      (equality ())

  and equality () =
    reduce_all_matches
      [ BANG_EQUAL; EQUAL_EQUAL ]
      (fun operator left -> Binary { left; operator; right = comparison () })
      (comparison ())

  and comparison () =
    reduce_all_matches
      [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ]
      (fun operator left -> Binary { left; operator; right = term () })
      (term ())

  and term () =
    reduce_all_matches [ MINUS; PLUS ]
      (fun operator left -> Binary { left; operator; right = factor () })
      (factor ())

  and factor () =
    reduce_all_matches [ SLASH; STAR ]
      (fun operator left -> Binary { left; operator; right = unary () })
      (unary ())

  and unary () =
    match ctx.tokens with
    | operator :: _ when match_any [ BANG; MINUS ] ->
        let right = unary () in
        Unary { operator; right }
    | _ -> call ()

  and call () =
    reduce_all_matches [ LEFT_PAREN ]
      (fun _ callee -> finish_call callee)
      (primary ())

  and finish_call callee =
    let rec consume_args args =
      if List.length args >= 255 then
        Error.of_error (List.hd ctx.tokens)
          "Can't have more than 255 arguments.";
      let args = expression () :: args in
      if match_any [ COMMA ] then consume_args @@ args else List.rev args
    in

    let args = if not @@ check RIGHT_PAREN then consume_args [] else [] in

    let paren = consume RIGHT_PAREN "Expect ')' after arguments." in
    Call { callee; paren; args }

  and primary () =
    match ctx.tokens with
    | _ when match_any [ FALSE ] -> Literal (Bool false)
    | _ when match_any [ TRUE ] -> Literal (Bool true)
    | _ when match_any [ NIL ] -> Literal Nil
    | TokenWithLiteral { literal = NumberLiteral num; _ } :: _
      when match_any [ NUMBER ] ->
        Literal (Num num)
    | TokenWithLiteral { literal = StringLiteral string; _ } :: _
      when match_any [ STRING ] ->
        Literal (String string)
    | variable :: _ when match_any [ IDENTIFIER ] -> Variable variable
    | _ when match_any [ LEFT_PAREN ] ->
        let expr = expression () in
        let _ = consume RIGHT_PAREN "Expect ')' after expression." in
        Grouping expr
    | token :: _ -> raise @@ ParseError (token, "Expect expression.")
    | [] -> raise Unreachable

  let rec to_stmts stmts =
    match ctx.tokens with
    | Token { kind = EOF; _ } :: [] -> List.rev stmts
    | _ ->
        to_stmts
          (match declaration () with
          | Error _ -> stmts
          | Ok stmt -> stmt :: stmts)

  and declaration () =
    try
      match ctx.tokens with
      | _ when match_any [ VAR ] -> Ok (var_declaration ())
      | _ -> Ok (statement ())
    with ParseError (token, msg) ->
      Error.of_error token msg;
      synchronize ();
      Error ()

  and var_declaration () =
    let name = consume IDENTIFIER "Expect variable name." in
    match ctx.tokens with
    | _ when match_any [ EQUAL ] ->
        let init = expression () in
        let _ = consume_semicolon "Expect ';' after variable declaration." in
        VarWithInit (name, init)
    | _ ->
        let _ = consume_semicolon "Expect ';' after variable declaration." in
        Var name

  and expression_stmt () =
    let expr = expression () in
    let _ = consume_semicolon "Expect ';' after expression." in
    Expression expr

  and block stmts =
    match ctx.tokens with
    | Token { kind = RIGHT_BRACE; _ } :: _ | Token { kind = EOF; _ } :: [] ->
        let _ = consume RIGHT_BRACE "Expect '}' after block." in
        List.rev stmts
    | _ ->
        block
          (match declaration () with
          | Error _ -> stmts
          | Ok stmt -> stmt :: stmts)

  and for_stmt () =
    let _ = consume LEFT_PAREN "Expect '(' after 'for'." in
    let init =
      match ctx.tokens with
      | _ when match_any [ SEMICOLON ] -> None
      | _ when match_any [ VAR ] -> Some (var_declaration ())
      | _ -> Some (expression_stmt ())
    in
    let condition =
      match ctx.tokens with
      | Token { kind = SEMICOLON; _ } :: _ -> Literal (Bool true)
      | _ -> expression ()
    in
    let _ = consume_semicolon "Expect ';' after loop condition." in
    let increment =
      match ctx.tokens with
      | Token { kind = RIGHT_PAREN; _ } :: _ -> None
      | _ -> Some (Expression (expression ()))
    in
    let _ = consume RIGHT_PAREN "Expect ')' after for clauses." in
    let body = statement () in
    match (init, condition, increment) with
    | None, condition, None -> While { condition; body }
    | Some init, condition, None -> Block [ init; While { condition; body } ]
    | None, condition, Some increment ->
        While { condition; body = Block [ body; increment ] }
    | Some init, condition, Some increment ->
        Block [ init; While { condition; body = Block [ body; increment ] } ]

  and statement () =
    match ctx.tokens with
    | _ when match_any [ LEFT_BRACE ] -> Block (block [])
    | _ when match_any [ PRINT ] ->
        let value = expression () in
        let _ = consume_semicolon "Expect ';' after value." in
        Print value
    | _ when match_any [ IF ] -> (
        let _ = consume LEFT_PAREN "Expect '(' after 'if'." in
        let condition = expression () in
        let _ = consume RIGHT_PAREN "Expect ')' after if condition." in
        let then_branch = statement () in
        match ctx.tokens with
        | _ when match_any [ ELSE ] ->
            let else_branch = Some (statement ()) in
            If { condition; then_branch; else_branch }
        | _ -> If { condition; then_branch; else_branch = None })
    | _ when match_any [ WHILE ] ->
        let _ = consume LEFT_PAREN "Expect '(' after 'while'." in
        let condition = expression () in
        let _ = consume RIGHT_PAREN "Expect ')' after while condition." in
        let body = statement () in
        While { condition; body }
    | _ when match_any [ FOR ] -> for_stmt ()
    | _ -> expression_stmt ()
end
