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
  | Var of var ref
  | Num of int

and var = { name : string; offset : int }

and func = {
  body : t list;
  locals : (string, var ref) Hashtbl.t;
  stack_size : int;
}

and t = { kind : kind; lhs : t option; rhs : t option }

let make kind = { kind; lhs = None; rhs = None }
let make_unary kind expr = { kind; lhs = Some expr; rhs = None }
let make_binary kind lhs rhs = { kind; lhs = Some lhs; rhs = Some rhs }
let make_num value = Num value |> make
let make_var var = Var var |> make
