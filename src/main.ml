open Error

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf
      "Usage: ./your_program.sh tokenize|parse|evaluate <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  let tokens filename =
    let chars =
      In_channel.with_open_text filename In_channel.input_all
      |> String.to_seq |> List.of_seq
    in
    Tokenize.tokenize chars [] 1
  in

  let parser tokens =
    (module Parser.Make (struct
      let tokens = tokens
    end) : Parser.Parser)
  in

  match command with
  | "tokenize" ->
      Token.set_trailing_zero true;
      Token.print @@ tokens filename
  | "parse" -> (
      Token.set_trailing_zero true;
      let (module P) = parser (tokens filename) in
      match P.to_expr () with
      | exception ParseError (token, msg) -> of_error token msg
      | expr -> expr |> Expr.to_string |> print_endline)
  | "evaluate" -> (
      Token.set_trailing_zero false;
      let (module P) = parser (tokens filename) in
      match P.to_expr () with
      | exception ParseError (token, msg) -> of_error token msg
      | expr -> (
          match Interpreter.evaluate expr with
          | exception RuntimeError (token, msg) -> of_runtime_error token msg
          | result -> print_endline @@ Expr.expr_literal_to_string result))
  | "run" -> (
      Token.set_trailing_zero false;
      let (module P) = parser (tokens filename) in
      match P.to_stmts [] with
      | exception ParseError (token, msg) -> of_error token msg
      | stmts -> (
          if !error then exit 65;
          try Interpreter.interpret_stmts stmts
          with RuntimeError (token, msg) -> of_runtime_error token msg))
  | unknown_command ->
      Printf.eprintf "Unknown command: %s\n" unknown_command;
      exit 1
;;

if !error then exit 65;
if !runtime_error then exit 70;
()
