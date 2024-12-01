type literal = NumberLiteral of float | StringLiteral of string

let trailing_zero = ref true
let set_trailing_zero v = trailing_zero := v

let number_to_string num =
  match !trailing_zero with
  | false ->
      let num_str = Float.to_string num in
      if Core.String.nget num_str (-1) = '.' then
        Core.String.slice num_str 0 (-1)
      else num_str
  | true ->
      if Float.is_integer num then Printf.sprintf "%.1f" num
      else Float.to_string num

let literal_to_string = function
  | StringLiteral s -> s
  | NumberLiteral n -> number_to_string n

type kind =
  (* Single-character tokens. *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* One or two character tokens. *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* Literals. *)
  | IDENTIFIER
  | STRING
  | NUMBER
  (* Keywords. *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

let kind_to_string = function
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"

type t = { kind : kind; lexeme : string; line : int; literal : literal option }

let to_string = function
  | { kind; literal = None; lexeme; _ } ->
      Printf.sprintf "%s %s %s" (kind_to_string kind) lexeme "null"
  | { kind; literal = Some literal; lexeme; _ } ->
      Printf.sprintf "%s %s %s" (kind_to_string kind) lexeme
        (literal_to_string literal)

let print tokens = Lib.(List.iter (to_string >> print_endline) tokens)
