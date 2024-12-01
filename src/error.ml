exception ParseError of Token.t * string
exception RuntimeError of Token.t * string
exception Todo
exception Unreachable

let error = ref false
let runtime_error = ref false

let report ref line where message =
  ref := true;
  Printf.eprintf "[line %d] Error%s: %s\n" line where message

let of_line line message = report error line "" message

let of_token ref token message =
  let open Token in
  match token with
  | { kind = EOF; line; _ } -> report ref line " at end" message
  | { line; lexeme; _ } -> report ref line (" at '" ^ lexeme ^ "'") message

let of_error = of_token error
let of_runtime_error = of_token runtime_error
