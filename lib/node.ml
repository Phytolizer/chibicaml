open Extensions

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
  func_locals : (string, var ref) Hashtbl.t; [@opaque]
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

let make tok kind = { kind; ty = None; tok; lhs = None; rhs = None }

let make_unary tok kind expr =
  { kind; ty = None; tok; lhs = Some expr; rhs = None }

let make_binary tok kind lhs rhs =
  { kind; ty = None; tok; lhs = Some lhs; rhs = Some rhs }

let make_num tok value = Num value |> make tok
let make_var tok var = Var var |> make tok

let rec add_type input (node : t) : t =
  match node.ty with
  | Some _ -> node
  | None ->
      let node = ref node in
      node := { !node with lhs = Option.map (add_type input) !node.lhs };
      node := { !node with rhs = Option.map (add_type input) !node.rhs };
      node := { !node with kind = kind_add_type input !node.kind };
      {
        !node with
        ty =
          (match !node.kind with
          | Add | Sub | Mul | Div ->
              let left_ty = Option.get (Option.get !node.lhs).ty in
              Some left_ty
          | Assign -> (
              let left = Option.get !node.lhs in
              let left_ty = Option.get left.ty in
              match left_ty.kind with
              | Array _ -> Token.error input left.tok "not an lvalue"
              | _ -> Some left_ty)
          | Eq | Ne | Lt | Le | Num _
          | FunCall _ (* TODO: should not always be int *) ->
              Some Type.int
          | Var var -> Some !var.var_ty
          | Addr -> (
              let left_ty = Option.get (Option.get !node.lhs).ty in
              match left_ty.kind with
              | Array (base, _) -> Some (Type.ptr_to base)
              | _ -> Some (Type.ptr_to left_ty))
          | Deref ->
              let left = Option.get !node.lhs in
              let left_ty = Option.get left.ty in
              Type.baseof left_ty
              |> Option.or_lazy
                   (lazy
                     (Token.error input left.tok "invalid pointer dereference"))
          | _ -> None);
      }

and kind_add_type input (kind : kind) : kind =
  match kind with
  | If node ->
      let node = ref node in
      node := { !node with if_cond = add_type input !node.if_cond };
      node := { !node with if_then_stmt = add_type input !node.if_then_stmt };
      node :=
        {
          !node with
          if_else_stmt = Option.map (add_type input) !node.if_else_stmt;
        };
      If !node
  | For node ->
      let node = ref node in
      node :=
        { !node with for_init = Option.map (add_type input) !node.for_init };
      node :=
        { !node with for_cond = Option.map (add_type input) !node.for_cond };
      node := { !node with for_inc = Option.map (add_type input) !node.for_inc };
      node := { !node with for_body = add_type input !node.for_body };
      For !node
  | Block node -> List.map (add_type input) node |> fun x -> Block x
  | kind -> kind

let make_add input tok lhs rhs =
  let lhs = add_type input lhs in
  let rhs = add_type input rhs in
  let lhs_ty = Option.get lhs.ty in
  let rhs_ty = Option.get rhs.ty in
  (* num + num *)
  if Type.is_int lhs_ty && Type.is_int rhs_ty then make_binary tok Add lhs rhs
  else if Type.is_ptr lhs_ty && Type.is_ptr rhs_ty then
    Token.error input tok "invalid operands"
  else
    (* swap if lhs is the int and rhs is the ptr *)
    let lhs, rhs = if Type.is_int lhs_ty then (rhs, lhs) else (lhs, rhs) in
    let lhs_ty = Option.get lhs.ty in
    let rhs =
      make_binary tok Mul rhs
        (make_num tok (Type.baseof lhs_ty |> Option.get |> fun x -> x.sizeof))
    in
    make_binary tok Add lhs rhs

let make_sub input tok lhs rhs =
  let lhs = add_type input lhs in
  let rhs = add_type input rhs in
  let lhs_ty = Option.get lhs.ty in
  let rhs_ty = Option.get rhs.ty in
  match (lhs_ty.kind, rhs_ty.kind) with
  (* num - num *)
  | Int, Int -> make_binary tok Sub lhs rhs
  (* ptr - num *)
  | Ptr _, Int ->
      let rhs =
        make_binary tok Mul rhs
          (make_num tok (Type.baseof lhs_ty |> Option.get |> fun x -> x.sizeof))
        |> add_type input
      in
      let node = make_binary tok Sub lhs rhs in
      { node with ty = Some lhs_ty }
  (* ptr - ptr *)
  | Ptr _, Ptr _ ->
      let node = make_binary tok Sub lhs rhs in
      let node = { node with ty = Some Type.int } in
      make_binary tok Div node
        (make_num tok (Type.baseof lhs_ty |> Option.get |> fun x -> x.sizeof))
  | _ -> Token.error input tok "invalid operands"
