let do_log = false

class type ['a] t = object
  val id : int
  val enclosing : 'a t option
  val mutable values : (string, 'a) Hashtbl.t
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
  let open Token in
  let open Error in
  object (self)
    val enclosing = enclosing
    val mutable values = Hashtbl.create 255

    method log s =
      if do_log then print_endline @@ Printf.sprintf "[env %d]: %s" id s

    method get name : 'a =
      let { lexeme; _ } = name in

      self#log @@ "get '" ^ lexeme ^ "'";

      match enclosing with
      | None -> (
          match Hashtbl.find_opt values lexeme with
          | None ->
              self#log @@ Printf.sprintf "Undefined variable '%s'." lexeme;

              raise
              @@ RuntimeError
                   (name, Printf.sprintf "%d Undefined variable '%s'." id lexeme)
          | Some value -> value)
      | Some enclosing -> (
          match Hashtbl.find_opt values lexeme with
          | None -> enclosing#get name
          | Some value -> value)

    method assign name value =
      let { lexeme; _ } = name in

      match enclosing with
      | None -> (
          match Hashtbl.find_opt values lexeme with
          | None ->
              self#log @@ Printf.sprintf "Undefined variable '%s'." lexeme;

              raise
              @@ RuntimeError
                   (name, Printf.sprintf "Undefined variable '%s'." lexeme)
          | Some _ ->
              (* self#log ("assigning to " ^ lexeme); *)
              Hashtbl.add values lexeme value)
      | Some enclosing -> (
          match Hashtbl.find_opt values lexeme with
          | None -> enclosing#assign name value
          | Some _ ->
              (* self#log ("in enclosing: assigning to " ^ lexeme); *)
              Hashtbl.add values lexeme value)

    method define name value =
      match enclosing with
      | None | Some _ ->
          self#log ("defining " ^ name);

          Hashtbl.add values name value

    method print_recursive =
      match enclosing with
      | None ->
          Hashtbl.iter
            (fun key value ->
              self#log @@ Printf.sprintf "%s -> %s" key (val_to_string value))
            values
      | Some enclosing ->
          Hashtbl.iter
            (fun key value ->
              self#log @@ Printf.sprintf "%s -> %s" key (val_to_string value))
            values;
          enclosing#print

    method print =
      Hashtbl.iter
        (fun key value ->
          self#log @@ Printf.sprintf "%s -> %s" key (val_to_string value))
        values
  end
