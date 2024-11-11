open Callable
open Error

type 'a env = 'a Env.t

type 'a interpreter =
  < get_globals : 'a env
  ; evaluate : expr -> expr_literal
  ; execute : stmt -> unit
  ; execute_block : stmt list -> 'a env -> unit
  ; interpret_stmts : stmt list -> unit >

and 'a lox_function = 'a interpreter -> expr_literal list -> unit

and expr_literal =
  | Callable of expr_literal lox_function callable
  | Num of float
  | String of string
  | Bool of bool
  | Nil

and expr =
  | Assign of { name : Token.t; value : expr }
  | Binary of { left : expr; operator : Token.t; right : expr }
  | Call of { callee : expr; paren : Token.t; args : expr list }
  | Grouping of expr
  | Literal of expr_literal
  | Logical of { left : expr; operator : Token.t; right : expr }
  | Unary of { operator : Token.t; right : expr }
  | Variable of Token.t

and stmt =
  | Print of expr
  | Expression of expr
  | Var of Token.t
  | VarWithInit of Token.t * expr
  | Block of stmt list
  | If of { condition : expr; then_branch : stmt; else_branch : stmt option }
  | While of { condition : expr; body : stmt }
  | Function of { name : Token.t; params : Token.t list; body : stmt list }

let expr_literal_to_string = function
  | Callable c -> c#to_string
  | Num num -> Token.literal_to_string (NumberLiteral num)
  | String string -> Token.literal_to_string (StringLiteral string)
  | Bool bool -> Printf.sprintf "%B" bool
  | Nil -> "nil"

let rec to_string = function
  | Literal literal -> expr_literal_to_string literal
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

let clock : (unit -> float) callable =
  object
    method arity = 0
    method call _ = 0.0
    method to_string = "<native fn>"
  end

let lox_function (d : stmt) : 'a lox_function callable =
  object
    method arity =
      match d with
      | Function { params; _ } -> List.length params
      | _ -> raise Unreachable

    method call i args =
      match d with
      | Function { params; body; _ } ->
          let env = i#get_globals in
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

          i#execute_block body env
      | _ -> raise Unreachable

    method to_string =
      match d with
      | Function { name = Token { lexeme; _ }; _ } -> "<fn " ^ lexeme ^ ">"
      | _ -> raise Unreachable
  end
