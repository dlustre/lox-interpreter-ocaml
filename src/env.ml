open Token
open Error
module StringMap = Map.Make (String)

class type ['a] t = object
  val enclosing : 'a t option
  val mutable values : 'a StringMap.t
  method get : Token.t -> 'a
  method assign : Token.t -> 'a -> unit
  method define : string -> 'a -> unit
  (* method print : int -> unit *)
end

class ['a] env enclosing =
  object
    val enclosing = enclosing
    val mutable values = StringMap.empty

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
                   (name, Printf.sprintf "Undefined variable '%s'." lexeme)
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
          | Some _ -> values <- StringMap.add lexeme value values)
      | Some enclosing -> (
          match StringMap.find_opt lexeme values with
          | None -> enclosing#assign name value
          | Some _ -> values <- StringMap.add lexeme value values)

    method define name value =
      match enclosing with
      | None | Some _ -> values <- StringMap.add name value values

    (* method print level =
       match enclosing with
       | None ->
           StringMap.iter
             (fun key value ->
               Printf.printf "[%d]: Key: %s, Value: %s\n" level key
                 (expr_literal_to_string value))
             values;
           ()
       | Some enclosing ->
           StringMap.iter
             (fun key value ->
               Printf.printf "[%d]: Key: %s, Value: %s\n" level key
                 (expr_literal_to_string value))
             values;
           enclosing#print level + 1;
           () *)
  end
