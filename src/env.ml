open Token
open Error
module StringMap = Map.Make (String)

let do_log = false

class type ['a] t = object
  val id : int
  val enclosing : 'a t option
  val mutable values : 'a StringMap.t
  method log : string -> unit
  method get : Token.t -> 'a
  method assign : Token.t -> 'a -> unit
  method define : string -> 'a -> unit
  method print : unit
end

class ['a] env enclosing =
  let counter = ref 0 in
  object (self)
    val id =
      incr counter;
      !counter

    val enclosing = enclosing
    val mutable values = StringMap.empty

    method log s =
      if do_log then print_endline @@ Printf.sprintf "[env %d]: %s" id s

    method get name : 'a =
      let lexeme =
        match name with
        | Token { lexeme; _ } | TokenWithLiteral { lexeme; _ } -> lexeme
      in

      match enclosing with
      | None -> (
          match StringMap.find_opt lexeme values with
          | None ->
              raise
              @@ RuntimeError
                   (name, Printf.sprintf "%d Undefined variable '%s'." id lexeme)
          | Some value -> value)
      | Some enclosing -> (
          match StringMap.find_opt lexeme values with
          | None -> enclosing#get name
          | Some value -> value)

    method assign name value =
      let lexeme =
        match name with
        | Token.Token { lexeme; _ } | Token.TokenWithLiteral { lexeme; _ } ->
            lexeme
      in
      match enclosing with
      | None -> (
          match StringMap.find_opt lexeme values with
          | None ->
              raise
              @@ RuntimeError
                   (name, Printf.sprintf "Undefined variable '%s'." lexeme)
          | Some _ ->
              (* self#log ("assigning to " ^ lexeme); *)
              values <- StringMap.add lexeme value values)
      | Some enclosing -> (
          match StringMap.find_opt lexeme values with
          | None -> enclosing#assign name value
          | Some _ ->
              (* self#log ("in enclosing: assigning to " ^ lexeme); *)
              values <- StringMap.add lexeme value values)

    method define name value =
      match enclosing with
      | None | Some _ ->
          self#log ("defining " ^ name);

          values <- StringMap.add name value values

    method print =
      match enclosing with
      | None ->
          StringMap.iter
            (fun key _value -> self#log @@ Printf.sprintf "Key: %s\n" key)
            values;
          ()
      | Some enclosing ->
          StringMap.iter
            (fun key _value -> self#log @@ Printf.sprintf "Key: %s\n" key)
            values;
          enclosing#print
  end
