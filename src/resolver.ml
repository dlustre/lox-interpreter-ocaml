let _do_log = false

type scope = (string, bool) Hashtbl.t

let scope_size = 255

let resolver interpreter =
  object (self)
    val interpreter = interpreter
    val scopes : scope Stack.t = Stack.create ()
    method top = Stack.top scopes

    method resolve_expr =
      let open Shared.Expr in
      function
      | Literal _ -> ()
      | Grouping expr | Unary { right = expr; _ } -> self#resolve_expr expr
      | Variable ({ lexeme; _ } as name) as expr ->
          if (not @@ Stack.is_empty scopes) && Hashtbl.find self#top lexeme then
            Error.of_error name
              "Can't read local variable in its own initializer."
          else self#resolve_local expr name
      | Assign { name; value } as expr ->
          self#resolve_expr value;
          self#resolve_local expr name
      | Binary { left; right; _ } | Logical { left; right; _ } ->
          self#resolve_expr left;
          self#resolve_expr right
      | Call { callee; args; _ } ->
          self#resolve_expr callee;
          List.iter self#resolve_expr args

    method resolve_stmt =
      let open Shared.Stmt in
      function
      | Expression stmt -> self#resolve_expr stmt
      | Print expr -> self#resolve_expr expr
      | Return { value = None; _ } -> ()
      | Return { value = Some value; _ } -> self#resolve_expr value
      | Block stmts ->
          self#begin_scope;
          self#resolve_stmts stmts;
          self#end_scope
      | Function { name; _ } as stmt ->
          self#declare name;
          self#define name;
          self#resolve_function stmt
      | If { condition; then_branch; else_branch } ->
          self#resolve_expr condition;
          self#resolve_stmt then_branch;
          if Option.is_some else_branch then
            self#resolve_stmt (Option.get else_branch)
      | While { condition; body } ->
          self#resolve_expr condition;
          self#resolve_stmt body
      | Var name ->
          self#declare name;
          self#define name
      | VarWithInit (name, init) ->
          self#declare name;
          self#resolve_expr init;
          self#define name

    method resolve_stmts stmts = List.iter self#resolve_stmt stmts

    method resolve_local expr name =
      let { lexeme; _ } : Token.t = name in
      match
        scopes |> Stack.to_seq
        |> Seq.find_index (fun scope ->
               Option.is_some @@ Hashtbl.find_opt scope lexeme)
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
      let new_scope : scope = Hashtbl.create scope_size in
      Stack.push new_scope scopes

    method end_scope =
      let _ = Stack.pop scopes in
      ()

    method declare name =
      if Stack.is_empty scopes then ()
      else
        let { lexeme; _ } : Token.t = name in

        let _ = Hashtbl.add self#top lexeme false in
        ()

    method define name =
      if Stack.is_empty scopes then ()
      else
        let { lexeme; _ } : Token.t = name in

        let _ = Hashtbl.add self#top lexeme true in
        ()
  end
