type t = { mutable depth : int }

let push (self : t) =
  print_endline "  push rax";
  self.depth <- self.depth + 1

let pop (self : t) arg =
  Printf.printf "  pop %s\n" arg;
  self.depth <- self.depth - 1

let gen_addr (node : Node.t) =
  match node.kind with
  | Node.Var name ->
      let offset = (Char.code name - Char.code 'a' + 1) * 8 in
      Printf.printf "  lea rax, [rbp - %d]\n" offset
  | _ -> Error.error "not an lvalue"

let rec gen_expr (self : t) (node : Node.t) =
  match node.kind with
  | Num value -> Printf.printf "  mov rax, %d\n" value
  | Var _ ->
      gen_addr node;
      print_endline "  mov rax, [rax]"
  | Assign ->
      gen_addr (Option.get node.lhs);
      push self;
      gen_expr self (Option.get node.rhs);
      pop self "rdi";
      print_endline "  mov [rdi], rax"
  | kind -> (
      gen_expr self (Option.get node.rhs);
      push self;
      gen_expr self (Option.get node.lhs);
      pop self "rdi";
      match kind with
      | Add -> print_endline "  add rax, rdi"
      | Sub -> print_endline "  sub rax, rdi"
      | Mul -> print_endline "  imul rax, rdi"
      | Div ->
          print_endline "  cqo";
          print_endline "  idiv rdi"
      | Eq | Ne | Lt | Le ->
          print_endline "  cmp rax, rdi";
          (match kind with
          | Eq -> print_endline "  sete al"
          | Ne -> print_endline "  setne al"
          | Lt -> print_endline "  setl al"
          | Le -> print_endline "  setle al"
          | _ -> Error.error "invalid expression");
          print_endline "  movzx rax, al"
      | _ -> Error.error "invalid expression")

let gen_stmt self (node : Node.t) =
  match node.kind with
  | Node.ExprStmt -> gen_expr self (Option.get node.lhs)
  | _ -> Error.error "invalid statement"

let gen (node : Node.t) =
  let self = { depth = 0 } in
  print_endline "  global main";
  print_endline "main:";
  print_endline "  push rbp";
  print_endline "  mov rbp, rsp";
  print_endline "  sub rsp, 208";
  (match node.kind with
  | Node.Prog contents -> List.iter (gen_stmt self) contents
  | _ -> Error.error "not a program");
  assert (self.depth == 0);
  print_endline "  mov rsp, rbp";
  print_endline "  pop rbp";
  print_endline "  ret"
