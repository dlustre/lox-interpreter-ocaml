let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  match command with
  | "tokenize" ->
      let file_contents =
        In_channel.with_open_text filename In_channel.input_all
      in
      let chars = file_contents |> String.to_seq |> List.of_seq in
      let tokens = Tokenize.tokenize chars [] 1 in
      Token.print tokens
  | "parse" -> (
      let file_contents =
        In_channel.with_open_text filename In_channel.input_all
      in
      let chars = file_contents |> String.to_seq |> List.of_seq in
      let tokens = Tokenize.tokenize chars [] 1 in
      let parser = Parse.parser tokens in

      match parser#to_expr with
      | Some expr -> expr |> Expr.to_string |> print_endline
      | None -> print_endline "Unexpected error")
  | unknown_command ->
      Printf.eprintf "Unknown command: %s\n" unknown_command;
      exit 1
;;

if !Error.hadError then exit 65;
if !Error.hadRuntimeError then exit 70;
()
