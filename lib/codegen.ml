type t = { mutable depth : int; mutable counter : int }

let push (self : t) =
  print_endline "  push rax";
  self.depth <- self.depth + 1

let pop (self : t) arg =
  Printf.printf "  pop %s\n" arg;
  self.depth <- self.depth - 1

let count (self : t) =
  let result = self.counter in
  self.counter <- self.counter + 1;
  result

let align_to (n : int) (align : int) : int = (n + align - 1) / align * align

let assign_lvar_offsets (prog : Node.func ref) =
  let offset = ref 0 in
  Hashtbl.iter
    (fun _ (var : Node.var ref) ->
      offset := !offset + 8;
      var := { !var with offset = !offset })
    !prog.func_locals;
  prog := { !prog with func_stack_size = align_to !offset 16 }

let gen_addr (node : Node.t) =
  match node.kind with
  | Node.Var var ->
      let offset = !var.offset in
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

let rec gen_stmt self (node : Node.t) =
  match node.kind with
  | Node.ExprStmt -> gen_expr self (Option.get node.lhs)
  | Node.Return ->
      gen_expr self (Option.get node.lhs);
      print_endline "  jmp .L.return"
  | Node.Block body -> List.iter (gen_stmt self) body
  | Node.If node ->
      let c = count self in
      gen_expr self node.if_cond;
      print_endline "  cmp rax, 0";
      Printf.printf "  je .L.else.%d\n" c;
      gen_stmt self node.if_then_stmt;
      Printf.printf "  jmp .L.end.%d\n" c;
      Printf.printf ".L.else.%d:\n" c;
      Option.map (gen_stmt self) node.if_else_stmt |> ignore;
      Printf.printf ".L.end.%d:\n" c
  | Node.For node ->
      let c = count self in
      Option.map (gen_stmt self) node.for_init |> ignore;
      Printf.printf ".L.begin.%d:\n" c;
      Option.map
        (fun cond ->
          gen_expr self cond;
          print_endline "  cmp rax, 0";
          Printf.printf "  je .L.end.%d\n" c)
        node.for_cond
      |> ignore;
      gen_stmt self node.for_body;
      Option.map (gen_expr self) node.for_inc |> ignore;
      Printf.printf "  jmp .L.begin.%d\n" c;
      Printf.printf ".L.end.%d:\n" c
  | _ -> Error.error "invalid statement"

let gen (prog : Node.func) =
  let prog = ref prog in
  assign_lvar_offsets prog;
  let self = { depth = 0; counter = 1 } in
  print_endline "  global main";
  print_endline "main:";
  print_endline "  push rbp";
  print_endline "  mov rbp, rsp";
  Printf.printf "  sub rsp, %d\n" !prog.func_stack_size;
  gen_stmt self !prog.func_body;
  print_endline ".L.return:";
  print_endline "  mov rsp, rbp";
  print_endline "  pop rbp";
  print_endline "  ret"
