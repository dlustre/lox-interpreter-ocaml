type lexeme = string
type line = int
type literal = NumberLiteral of float | StringLiteral of string

type token =
  | Token of lexeme * line
  | TokenWithLiteral of literal * lexeme * line

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let is_alphanumeric = function
  | x when is_alpha x || is_digit x -> true
  | _ -> false

let hadError = ref false
let hadRuntimeError = ref false

let error line message =
  hadError := true;
  Printf.eprintf "[line %d] Error: %s\n" line message

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

let stringify_token_lexeme = function
  | "(" -> "LEFT_PAREN"
  | ")" -> "RIGHT_PAREN"
  | "{" -> "LEFT_BRACE"
  | "}" -> "RIGHT_BRACE"
  | "," -> "COMMA"
  | "." -> "DOT"
  | "-" -> "MINUS"
  | "+" -> "PLUS"
  | ";" -> "SEMICOLON"
  | "/" -> "SLASH"
  | "*" -> "STAR"
  | "=" -> "EQUAL"
  | "==" -> "EQUAL_EQUAL"
  | "!=" -> "BANG_EQUAL"
  | "!" -> "BANG"
  | "<" -> "LESS"
  | "<=" -> "LESS_EQUAL"
  | ">" -> "GREATER"
  | ">=" -> "GREATER_EQUAL"
  | str_literal when String.starts_with ~prefix:"\"" str_literal -> "STRING"
  | num_literal
    when String.length num_literal > 0 && is_digit (String.get num_literal 0) ->
      "NUMBER"
  | identifier
    when String.length identifier > 0 && is_alpha (String.get identifier 0) ->
      if List.exists (fun i -> i = identifier) keywords then
        String.uppercase_ascii identifier
      else "IDENTIFIER"
  | "" -> "EOF"
  | _ -> "UNKNOWN"

let stringify = function
  | Token (lexeme, _) ->
      Printf.sprintf "%s %s %s" (stringify_token_lexeme lexeme) lexeme "null"
  | TokenWithLiteral (literal, lexeme, _) ->
      Printf.sprintf "%s %s %s"
        (stringify_token_lexeme lexeme)
        lexeme
        (match literal with
        | NumberLiteral f ->
            Printf.sprintf (if Float.is_integer f then "%.1f" else "%.15g") f
        | StringLiteral s -> s)

let rec number = function
  | digit :: rest, after_dot, literal when is_digit digit ->
      number (rest, after_dot, digit :: literal)
  | '.' :: rest, false, literal -> number (rest, true, '.' :: literal)
  | after_num, _, literal ->
      (literal |> List.rev |> List.to_seq |> String.of_seq, after_num)

let rec tokenize chars tokens line =
  match chars with
  | [] -> List.rev (Token ("", line) :: tokens)
  | digit :: _ as chars_with_digit when is_digit digit ->
      let num_literal_str, rest = number (chars_with_digit, false, []) in
      tokenize rest
        (TokenWithLiteral
           ( NumberLiteral (Float.of_string num_literal_str),
             num_literal_str,
             line )
        :: tokens)
        line
  | '"' :: rest ->
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

      let result =
        match consume_str_literal rest [] with
        | Some (StringLiteral str_literal, after_str) ->
            tokenize after_str
              (TokenWithLiteral
                 ( StringLiteral str_literal,
                   Printf.sprintf "\"%s\"" str_literal,
                   line )
              :: tokens)
              line
        | _ ->
            error line "Unterminated string.";
            tokenize [] tokens line
      in
      result
  | ' ' :: rest | '\r' :: rest | '\t' :: rest -> tokenize rest tokens line
  | '\n' :: rest -> tokenize rest tokens (line + 1)
  | '/' :: '/' :: rest ->
      let rec consume_comment = function
        | ([] | '\n' :: _) as after_comment -> after_comment
        | _ :: rest -> consume_comment rest
      in
      tokenize (consume_comment rest) tokens line
  | '<' :: '=' :: rest -> tokenize rest (Token ("<=", 0) :: tokens) line
  | '>' :: '=' :: rest -> tokenize rest (Token (">=", 0) :: tokens) line
  | '!' :: '=' :: rest -> tokenize rest (Token ("!=", 0) :: tokens) line
  | '=' :: '=' :: rest -> tokenize rest (Token ("==", 0) :: tokens) line
  | char :: rest when List.exists (fun c -> c = char) single_chars ->
      tokenize rest (Token (String.make 1 char, line) :: tokens) line
  | alpha_char :: _ as chars_with_identifier when is_alpha alpha_char ->
      let rec consume_identifier = function
        | alnum_char :: rest, identifier when is_alphanumeric alnum_char ->
            consume_identifier (rest, alnum_char :: identifier)
        | rest, identifier ->
            (identifier |> List.rev |> List.to_seq |> String.of_seq, rest)
      in
      let identifier, rest = consume_identifier (chars_with_identifier, []) in

      tokenize rest (Token (identifier, line) :: tokens) line
  | unknown_char :: rest ->
      error line (Printf.sprintf "Unexpected character: %c" unknown_char);
      tokenize rest tokens line

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in
  let chars = file_contents |> String.to_seq |> List.of_seq in
  let tokens = tokenize chars [] 1 in
  List.iter (fun x -> x |> stringify |> print_endline) tokens;
  if !hadError then exit 65;
  if !hadRuntimeError then exit 70;
  ()
