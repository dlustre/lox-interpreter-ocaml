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

  match command with
  | "tokenize" ->
      Token.set_trailing_zero true;
      Token.print @@ tokens filename
  | "parse" -> (
      Token.set_trailing_zero true;
      match (Parse.parser @@ tokens filename)#to_expr with
      | exception Parse.ParseError (token, msg) ->
          Error.of_token Error.error token msg
      | expr -> expr |> Expr.to_string |> print_endline)
  | "evaluate" -> (
      Token.set_trailing_zero false;
      match (Parse.parser @@ tokens filename)#to_expr with
      | exception Parse.ParseError (token, msg) ->
          Error.of_token Error.error token msg
      | expr -> (
          let interpreter = Interpreter.interpreter in
          match interpreter#evaluate expr with
          | exception Interpreter.RuntimeError (token, msg) ->
              Error.of_token Error.runtime_error token msg
          | result -> print_endline @@ Expr.expr_literal_to_string result))
  | unknown_command ->
      Printf.eprintf "Unknown command: %s\n" unknown_command;
      exit 1
;;

if !Error.error then exit 65;
if !Error.runtime_error then exit 70;
()
