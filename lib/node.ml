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
  | ExprStmt
  (* while loops go here too *)
  | For of for_stmt
  | If of if_stmt
  | Return
  | Var of var ref
  | Num of int
  | Block of t list

and var = { name : string; offset : int }

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
and t = { kind : kind; tok : Token.t; lhs : t option; rhs : t option }

let make tok kind = { kind; tok; lhs = None; rhs = None }
let make_unary tok kind expr = { kind; tok; lhs = Some expr; rhs = None }
let make_binary tok kind lhs rhs = { kind; tok; lhs = Some lhs; rhs = Some rhs }
let make_num tok value = Num value |> make tok
let make_var tok var = Var var |> make tok
