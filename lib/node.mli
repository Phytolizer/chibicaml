type kind =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | Assign
  | Addr
  | Deref
  | FunCall of string
  | ExprStmt
  (* while loops go here too *)
  | For of for_stmt
  | If of if_stmt
  | Return
  | Var of var ref
  | Num of int
  | Block of t list

and var = { name : string; var_ty : Type.t; offset : int }

and func = {
  func_body : t;
  func_locals : (string, var ref) Hashtbl.t;
  func_stack_size : int;
}

and for_stmt = {
  for_init : t option;
  for_cond : t option;
  for_inc : t option;
  for_body : t;
}

and if_stmt = { if_cond : t; if_then_stmt : t; if_else_stmt : t option }

and t = {
  kind : kind;
  ty : Type.t option;
  tok : Token.t;
  lhs : t option;
  rhs : t option;
}

val make : Token.t -> kind -> t
val make_unary : Token.t -> kind -> t -> t
val make_binary : Token.t -> kind -> t -> t -> t
val make_num : Token.t -> int -> t
val make_var : Token.t -> var ref -> t
val make_add : string -> Token.t -> t -> t -> t
val make_sub : string -> Token.t -> t -> t -> t
val add_type : t -> t
