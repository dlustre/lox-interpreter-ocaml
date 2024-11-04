let hadError = ref false
let hadRuntimeError = ref false

let error line message =
  hadError := true;
  Printf.eprintf "[line %d] Error: %s\n" line message
