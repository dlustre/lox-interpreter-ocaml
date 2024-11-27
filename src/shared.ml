open Callable

module rec Interpreter : sig
  type 'a t =
    < get_globals : 'a Env.t
    ; evaluate : Expr.t -> Expr.literal
    ; execute : Stmt.t -> unit
    ; execute_block : Stmt.t list -> 'a Env.t -> unit
    ; interpret_stmts : Stmt.t list -> unit >
end =
  Interpreter

and LoxFunction : sig
  type t = Expr.literal Interpreter.t -> Expr.literal list -> Expr.literal

  val make : Stmt.t -> Expr.literal Env.t -> t callable
end = struct
  exception NotAFunction

  type t = Expr.literal Interpreter.t -> Expr.literal list -> Expr.literal

  let make (declaration : Stmt.t) (closure : Expr.literal Env.t) =
    object
      method arity =
        match declaration with
        | Function { params; _ } -> List.length params
        | _ -> raise NotAFunction

      method call interpreter args =
        match declaration with
        | Function { params; body; _ } -> (
            let env = Oo.copy closure in
            env#print;
            let zipped = Seq.zip (List.to_seq params) (List.to_seq args) in
            let _ =
              Seq.iter
                (fun (param, arg) ->
                  let lexeme =
                    match param with
                    | Token.Token { lexeme; _ }
                    | Token.TokenWithLiteral { lexeme; _ } ->
                        lexeme
                  in
                  env#define lexeme arg)
                zipped
            in

            try
              interpreter#execute_block body env;
              Expr.Nil
            with Expr.Return value -> value)
        | _ -> raise NotAFunction

      method to_string =
        match declaration with
        | Function { name = Token { lexeme; _ }; _ } -> "<fn " ^ lexeme ^ ">"
        | _ -> raise NotAFunction
    end
end

and Expr : sig
  type t =
    | Assign of { name : Token.t; value : t }
    | Binary of { left : t; operator : Token.t; right : t }
    | Call of { callee : t; paren : Token.t; args : t list }
    | Grouping of t
    | Literal of literal
    | Logical of { left : t; operator : Token.t; right : t }
    | Unary of { operator : Token.t; right : t }
    | Variable of Token.t

  and literal =
    | Callable of LoxFunction.t callable
    | Num of float
    | String of string
    | Bool of bool
    | Nil

  exception Return of literal

  val literal_to_string : literal -> string
  val to_string : t -> string
end = struct
  type t =
    | Assign of { name : Token.t; value : t }
    | Binary of { left : t; operator : Token.t; right : t }
    | Call of { callee : t; paren : Token.t; args : t list }
    | Grouping of t
    | Literal of literal
    | Logical of { left : t; operator : Token.t; right : t }
    | Unary of { operator : Token.t; right : t }
    | Variable of Token.t

  and literal =
    | Callable of LoxFunction.t callable
    | Num of float
    | String of string
    | Bool of bool
    | Nil

  exception Return of literal

  let literal_to_string = function
    | Callable c -> c#to_string
    | Num num -> Token.literal_to_string (NumberLiteral num)
    | String string -> Token.literal_to_string (StringLiteral string)
    | Bool bool -> Printf.sprintf "%B" bool
    | Nil -> "nil"

  let rec to_string = function
    | Literal literal -> literal_to_string literal
    | Grouping expr -> parenthesize ("group", [ expr ], "")
    | Unary { operator = Token { lexeme; _ }; right } ->
        parenthesize (lexeme, [ right ], "")
    | Binary { left; operator = Token { lexeme; _ }; right } ->
        parenthesize (lexeme, [ left; right ], "")
    | _ -> "Unknown Expr."

  and parenthesize = function
    | name, exprs, "" -> parenthesize (name, exprs, "(" ^ name)
    | _, [], acc -> acc ^ ")"
    | name, expr :: rest, acc ->
        parenthesize (name, rest, acc ^ " " ^ to_string expr)
end

and Stmt : sig
  type t =
    | Print of Expr.t
    | Expression of Expr.t
    | Var of Token.t
    | VarWithInit of Token.t * Expr.t
    | Block of t list
    | If of { condition : Expr.t; then_branch : t; else_branch : t option }
    | While of { condition : Expr.t; body : t }
    | Function of { name : Token.t; params : Token.t list; body : t list }
    | Return of { keyword : Token.t; value : Expr.t option }
end =
  Stmt