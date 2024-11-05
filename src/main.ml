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
  | "tokenize" -> Token.print @@ tokens filename
  | "parse" -> (
      match (Parse.parser @@ tokens filename)#to_expr with
      | exception Parse.ParseError (token, msg) -> Error.of_token token msg
      | expr -> expr |> Expr.to_string |> print_endline)
  | "evaluate" -> (
      match (Parse.parser @@ tokens filename)#to_expr with
      | exception Parse.ParseError (token, msg) -> Error.of_token token msg
      | expr ->
          let interpreter = Interpreter.interpreter in
          let result = interpreter#evaluate expr in
          result |> Expr.to_string |> print_endline)
  | unknown_command ->
      Printf.eprintf "Unknown command: %s\n" unknown_command;
      exit 1
;;

if !Error.hadError then exit 65;
if !Error.hadRuntimeError then exit 70;
()
