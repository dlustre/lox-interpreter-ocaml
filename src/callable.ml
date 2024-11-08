module type Callable = sig
  type t

  val arity : t -> int
  val to_string : t -> string
end

module Clock : Callable = struct
  type t = unit

  let arity _ = 0
  let to_string _ = "<native fn>"
end
