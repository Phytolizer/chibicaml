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

and var = { name : string; var_ty : Type.t; offset : int }

and func = {
  func_body : t;
  func_locals : (string, var ref) Hashtbl.t;
  func_stack_size : int;
}

and funcall = { funcall_name : string; funcall_args : t list }

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

let make tok kind = { kind; ty = None; tok; lhs = None; rhs = None }

let make_unary tok kind expr =
  { kind; ty = None; tok; lhs = Some expr; rhs = None }

let make_binary tok kind lhs rhs =
  { kind; ty = None; tok; lhs = Some lhs; rhs = Some rhs }

let make_num tok value = Num value |> make tok
let make_var tok var = Var var |> make tok

let rec add_type (node : t) : t =
  match node.ty with
  | Some _ -> node
  | None ->
      let node = ref node in
      node := { !node with lhs = Option.map add_type !node.lhs };
      node := { !node with rhs = Option.map add_type !node.rhs };
      node := { !node with kind = kind_add_type !node.kind };
      node :=
        {
          !node with
          ty =
            (match !node.kind with
            | Add | Sub | Mul | Div | Assign ->
                let left_ty = Option.get (Option.get !node.lhs).ty in
                Some left_ty
            | Eq | Ne | Lt | Le | Var _ | Num _ | FunCall _ ->
                Some (Type.make Int)
            | Addr ->
                let left_ty = Option.get (Option.get !node.lhs).ty in
                Some (Type.ptr_to left_ty)
            | Deref ->
                let left_ty = Option.get (Option.get !node.lhs).ty in
                Some
                  (match left_ty.kind with
                  | Type.Ptr base -> base
                  | _ -> Type.make Int)
            | _ -> None);
        };
      !node

and kind_add_type (kind : kind) : kind =
  match kind with
  | If node ->
      let node = ref node in
      node := { !node with if_cond = add_type !node.if_cond };
      node := { !node with if_then_stmt = add_type !node.if_then_stmt };
      node :=
        { !node with if_else_stmt = Option.map add_type !node.if_else_stmt };
      If !node
  | For node ->
      let node = ref node in
      node := { !node with for_init = Option.map add_type !node.for_init };
      node := { !node with for_cond = Option.map add_type !node.for_cond };
      node := { !node with for_inc = Option.map add_type !node.for_inc };
      node := { !node with for_body = add_type !node.for_body };
      For !node
  | Block node -> List.map add_type node |> fun x -> Block x
  | kind -> kind

let make_add input tok lhs rhs =
  let lhs = add_type lhs in
  let rhs = add_type rhs in
  let lhs_ty = Option.get lhs.ty in
  let rhs_ty = Option.get rhs.ty in
  (* num + num *)
  if Type.is_int lhs_ty && Type.is_int rhs_ty then make_binary tok Add lhs rhs
  else if Type.is_ptr lhs_ty && Type.is_ptr rhs_ty then
    Token.error input tok "invalid operands"
  else
    (* swap if lhs is the int and rhs is the ptr *)
    let lhs, rhs = if Type.is_int lhs_ty then (rhs, lhs) else (lhs, rhs) in
    let rhs = make_binary tok Mul rhs (make_num tok 8) in
    make_binary tok Add lhs rhs

let make_sub input tok lhs rhs =
  let lhs = add_type lhs in
  let rhs = add_type rhs in
  let lhs_ty = Option.get lhs.ty in
  let rhs_ty = Option.get rhs.ty in
  match (lhs_ty.kind, rhs_ty.kind) with
  (* num - num *)
  | Int, Int -> make_binary tok Sub lhs rhs
  (* ptr - num *)
  | Ptr _, Int ->
      let rhs = make_binary tok Mul rhs (make_num tok 8) |> add_type in
      let node = make_binary tok Sub lhs rhs in
      { node with ty = Some lhs_ty }
  (* ptr - ptr *)
  | Ptr _, Ptr _ ->
      let node = make_binary tok Sub lhs rhs in
      let node = { node with ty = Some (Type.make Int) } in
      make_binary tok Div node (make_num tok 8)
  | _ -> Token.error input tok "invalid operands"
