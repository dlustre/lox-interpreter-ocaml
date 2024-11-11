open Error

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf
      "Usage: ./your_program.sh tokenize|parse|evaluate <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  let tokens filename =
    Tokenize.tokenize
      (In_channel.input_all
      |> In_channel.with_open_text filename
      |> String.to_seq |> List.of_seq)
      [] 1
  in

  match command with
  | "tokenize" ->
      Token.set_trailing_zero true;
      Token.print @@ tokens filename
  | "parse" -> (
      Token.set_trailing_zero true;
      match (new Parser.parser @@ tokens filename)#to_expr with
      | exception ParseError (token, msg) -> of_error token msg
      | expr -> expr |> Shared.Expr.to_string |> print_endline)
  | "evaluate" -> (
      Token.set_trailing_zero false;
      match (new Parser.parser @@ tokens filename)#to_expr with
      | exception ParseError (token, msg) -> of_error token msg
      | expr -> (
          match Interpreter.interpreter#evaluate expr with
          | exception RuntimeError (token, msg) -> of_runtime_error token msg
          | result -> print_endline @@ Shared.Expr.literal_to_string result))
  | "run" -> (
      Token.set_trailing_zero false;
      match (new Parser.parser @@ tokens filename)#to_stmts [] with
      | exception ParseError (token, msg) -> of_error token msg
      | stmts -> (
          if !error then exit 65;
          try Interpreter.interpreter#interpret_stmts stmts
          with RuntimeError (token, msg) -> of_runtime_error token msg))
  | unknown_command ->
      Printf.eprintf "Unknown command: %s\n" unknown_command;
      exit 1
;;

if !error then exit 65;
if !runtime_error then exit 70;
()
