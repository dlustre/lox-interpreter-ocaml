let _do_log = false

let resolver interpreter =
  object (self)
    val interpreter = interpreter
    val scopes : bool Env.StringMap.t Stack.t = Stack.create ()
    method top = Stack.top scopes

    method resolve_expr =
      let open Shared.Expr in
      function
      | Variable ({ lexeme; _ } as name) as expr ->
          if
            (not (Stack.is_empty scopes))
            && self#top |> Env.StringMap.find lexeme = false
          then
            Error.of_error name
              "Can't read local variable in its own initializer."
          else self#resolve_local expr name
      | Assign { name; value } as expr ->
          self#resolve_expr value;
          self#resolve_local expr name
      | _ -> raise Error.Todo

    method resolve_stmt =
      let open Shared.Stmt in
      function
      | Block stmts ->
          self#begin_scope;
          self#resolve_stmts stmts;
          self#end_scope
      | Function { name; _ } as stmt ->
          self#declare name;
          self#define name;
          self#resolve_function stmt
      | Var name ->
          self#declare name;
          self#define name
      | VarWithInit (name, init) ->
          self#declare name;
          self#resolve_expr init;
          self#define name
      | _ -> raise Error.Todo

    method resolve_stmts stmts = List.iter self#resolve_stmt stmts

    method resolve_local expr name =
      let { lexeme; _ } : Token.t = name in
      match
        scopes |> Stack.to_seq
        |> Seq.find_index (fun scope ->
               Env.StringMap.exists (fun str _ -> str = lexeme) scope)
      with
      | None -> ()
      | Some i -> interpreter#resolve expr (Stack.length scopes - 1 - i)

    method resolve_function =
      function
      | Function { params; body; _ } ->
          self#begin_scope;
          List.iter
            (fun param ->
              self#declare param;
              self#define param)
            params;
          self#resolve_stmts body;
          self#end_scope
      | _ -> raise Error.Unreachable

    method begin_scope =
      let new_scope : bool Env.StringMap.t = Env.StringMap.empty in
      Stack.push new_scope scopes

    method end_scope =
      let _ = Stack.pop scopes in
      ()

    method declare name =
      if Stack.is_empty scopes then ()
      else
        let { lexeme; _ } : Token.t = name in

        let _ = self#top |> Env.StringMap.add lexeme false in
        ()

    method define name =
      if Stack.is_empty scopes then ()
      else
        let { lexeme; _ } : Token.t = name in

        let _ = self#top |> Env.StringMap.add lexeme true in
        ()
  end
