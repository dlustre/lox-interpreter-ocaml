open Token

exception UnexpectedLexeme

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let is_alphanumeric = function
  | x when is_alpha x || is_digit x -> true
  | _ -> false

let single_chars =
  [ '('; ')'; '{'; '}'; ','; '.'; '-'; '+'; ';'; '*'; '='; '!'; '<'; '>'; '/' ]

let keywords =
  [
    "and";
    "class";
    "else";
    "for";
    "fun";
    "if";
    "nil";
    "or";
    "print";
    "return";
    "super";
    "this";
    "true";
    "var";
    "while";
    "false";
  ]

let lexeme_to_token_kind = function
  | "(" -> LEFT_PAREN
  | ")" -> RIGHT_PAREN
  | "{" -> LEFT_BRACE
  | "}" -> RIGHT_BRACE
  | "," -> COMMA
  | "." -> DOT
  | "-" -> MINUS
  | "+" -> PLUS
  | ";" -> SEMICOLON
  | "/" -> SLASH
  | "*" -> STAR
  | "=" -> EQUAL
  | "==" -> EQUAL_EQUAL
  | "!=" -> BANG_EQUAL
  | "!" -> BANG
  | "<" -> LESS
  | "<=" -> LESS_EQUAL
  | ">" -> GREATER
  | ">=" -> GREATER_EQUAL
  | "and" -> AND
  | "class" -> CLASS
  | "else" -> ELSE
  | "false" -> FALSE
  | "fun" -> FUN
  | "for" -> FOR
  | "if" -> IF
  | "nil" -> NIL
  | "or" -> OR
  | "print" -> PRINT
  | "return" -> RETURN
  | "super" -> SUPER
  | "this" -> THIS
  | "true" -> TRUE
  | "var" -> VAR
  | "while" -> WHILE
  | identifier
    when String.length identifier > 0 && is_alpha (String.get identifier 0) ->
      IDENTIFIER
  | _ -> raise UnexpectedLexeme

let rec number = function
  | digit :: rest, after_dot, literal when is_digit digit ->
      number (rest, after_dot, digit :: literal)
  | '.' :: rest, false, literal -> number (rest, true, '.' :: literal)
  | after_num, _, literal ->
      (literal |> List.rev |> List.to_seq |> String.of_seq, after_num)

let rec tokenize chars tokens line =
  match chars with
  | [] -> List.rev (Token { kind = EOF; lexeme = ""; line } :: tokens)
  | digit :: _ as chars_with_digit when is_digit digit ->
      let lexeme, rest = number (chars_with_digit, false, []) in
      tokenize rest
        (TokenWithLiteral
           {
             kind = NUMBER;
             literal = NumberLiteral (Float.of_string lexeme);
             lexeme;
             line;
           }
        :: tokens)
        line
  | '"' :: rest -> (
      let rec consume_str_literal c literal =
        match c with
        | [] -> None
        | '"' :: after_str ->
            Some
              ( StringLiteral
                  (literal |> List.rev |> List.to_seq |> String.of_seq),
                after_str )
        | str_char :: after_char ->
            consume_str_literal after_char (str_char :: literal)
      in

      match consume_str_literal rest [] with
      | Some (StringLiteral str_literal, after_str) ->
          tokenize after_str
            (TokenWithLiteral
               {
                 kind = STRING;
                 literal = StringLiteral str_literal;
                 lexeme = Printf.sprintf "\"%s\"" str_literal;
                 line;
               }
            :: tokens)
            line
      | _ ->
          Error.of_line Error.error line "Unterminated string.";
          tokenize [] tokens line)
  | ' ' :: rest | '\r' :: rest | '\t' :: rest -> tokenize rest tokens line
  | '\n' :: rest -> tokenize rest tokens (line + 1)
  | '/' :: '/' :: rest ->
      let rec consume_comment = function
        | ([] | '\n' :: _) as after_comment -> after_comment
        | _ :: rest -> consume_comment rest
      in
      tokenize (consume_comment rest) tokens line
  | first_char :: '=' :: rest
    when List.exists (fun c -> c = first_char) [ '<'; '>'; '!'; '=' ] ->
      let lexeme = [ first_char; '=' ] |> List.to_seq |> String.of_seq in
      let kind = lexeme_to_token_kind lexeme in
      tokenize rest (Token { kind; lexeme; line } :: tokens) line
  | char :: rest when List.exists (fun c -> c = char) single_chars ->
      let lexeme = String.make 1 char in
      let kind = lexeme_to_token_kind lexeme in
      tokenize rest (Token { kind; lexeme; line } :: tokens) line
  | alpha_char :: _ as chars_with_identifier when is_alpha alpha_char ->
      let rec consume_identifier = function
        | alnum_char :: rest, identifier when is_alphanumeric alnum_char ->
            consume_identifier (rest, alnum_char :: identifier)
        | rest, identifier ->
            (identifier |> List.rev |> List.to_seq |> String.of_seq, rest)
      in
      let lexeme, rest = consume_identifier (chars_with_identifier, []) in
      let kind = lexeme_to_token_kind lexeme in
      tokenize rest (Token { kind; lexeme; line } :: tokens) line
  | unknown_char :: rest ->
      Error.of_line Error.error line
        (Printf.sprintf "Unexpected character: %c" unknown_char);
      tokenize rest tokens line
