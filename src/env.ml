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
  method print_recursive : unit
  method print : unit
end

let counter = ref 0

let get_counter () =
  incr counter;
  !counter

class ['a] env enclosing val_to_string id =
  object (self)
    val enclosing = enclosing
    val mutable values = StringMap.empty

    method log s =
      if do_log then print_endline @@ Printf.sprintf "[env %d]: %s" id s

    method get name : 'a =
      let lexeme =
        match name with
        | Token { lexeme; _ } | TokenWithLiteral { lexeme; _ } -> lexeme
      in

      self#log @@ "get '" ^ lexeme ^ "'";

      match enclosing with
      | None -> (
          match StringMap.find_opt lexeme values with
          | None ->
              self#log @@ Printf.sprintf "Undefined variable '%s'." lexeme;

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
              self#log @@ Printf.sprintf "Undefined variable '%s'." lexeme;

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

    method print_recursive =
      match enclosing with
      | None ->
          StringMap.iter
            (fun key value ->
              self#log @@ Printf.sprintf "%s -> %s" key (val_to_string value))
            values
      | Some enclosing ->
          StringMap.iter
            (fun key value ->
              self#log @@ Printf.sprintf "%s -> %s" key (val_to_string value))
            values;
          enclosing#print

    method print =
      StringMap.iter
        (fun key value ->
          self#log @@ Printf.sprintf "%s -> %s" key (val_to_string value))
        values
  end
