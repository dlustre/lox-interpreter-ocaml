open Token

let hadError = ref false
let hadRuntimeError = ref false

let report line where message =
  hadError := true;
  Printf.eprintf "[line %d] Error%s: %s\n" line where message

let of_line line message = report line "" message

let of_token token message =
  match token with
  | Token { kind = EOF; line; _ } -> report line " at end" message
  | Token { line; lexeme; _ } | TokenWithLiteral { line; lexeme; _ } ->
      report line (" at '" ^ lexeme ^ "'") message
