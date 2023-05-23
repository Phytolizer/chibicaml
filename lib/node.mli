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
  | FunCall of funcall
  | ExprStmt
  (* while loops go here too *)
  | For of for_stmt
  | If of if_stmt
  | Return
  | Var of var ref
  | Num of int
  | Block of t list
[@@deriving show]

and var = { name : string; var_ty : Type.t; offset : int } [@@deriving show]

and func = {
  func_name : string;
  func_params : var ref list;
  func_body : t;
  func_locals : (string, var ref) Hashtbl.t;
  func_stack_size : int;
}
[@@deriving show]

and funcall = { funcall_name : string; funcall_args : t list } [@@deriving show]

and for_stmt = {
  for_init : t option;
  for_cond : t option;
  for_inc : t option;
  for_body : t;
}
[@@deriving show]

and if_stmt = { if_cond : t; if_then_stmt : t; if_else_stmt : t option }
[@@deriving show]

and t = {
  kind : kind;
  ty : Type.t option;
  tok : Token.t;
  lhs : t option;
  rhs : t option;
}
[@@deriving show]

and prog = func list

val make : Token.t -> kind -> t
val make_unary : Token.t -> kind -> t -> t
val make_binary : Token.t -> kind -> t -> t -> t
val make_num : Token.t -> int -> t
val make_var : Token.t -> var ref -> t
val make_add : string -> Token.t -> t -> t -> t
val make_sub : string -> Token.t -> t -> t -> t
val add_type : string -> t -> t
