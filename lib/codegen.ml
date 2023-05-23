type t = { mutable depth : int; mutable counter : int }

let emit text =
  print_char '\t';
  print_endline text

let emitf text = Printf.ksprintf (fun s -> Printf.printf "  %s\n" s) text

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

let rec gen_addr (self : t) (input : string) (node : Node.t) =
  match node.kind with
  | Node.Var var ->
      let offset = !var.offset in
      emitf "lea rax, [rbp - %d]" offset
  | Node.Deref -> gen_expr self input (Option.get node.lhs)
  | _ -> Token.error input node.tok "not an lvalue"

and gen_expr (self : t) (input : string) (node : Node.t) =
  match node.kind with
  | Num value -> Printf.printf "  mov rax, %d\n" value
  | Var _ ->
      gen_addr self input node;
      emit "mov rax, [rax]"
  | Deref ->
      gen_expr self input (Option.get node.lhs);
      emit "mov rax, [rax]"
  | Addr -> gen_addr self input (Option.get node.lhs)
  | Assign ->
      gen_addr self input (Option.get node.lhs);
      push self;
      gen_expr self input (Option.get node.rhs);
      pop self "rdi";
      emit "mov [rdi], rax"
  | kind -> (
      gen_expr self input (Option.get node.rhs);
      push self;
      gen_expr self input (Option.get node.lhs);
      pop self "rdi";
      match kind with
      | Add -> emit "add rax, rdi"
      | Sub -> emit "sub rax, rdi"
      | Mul -> emit "imul rax, rdi"
      | Div ->
          emit "cqo";
          emit "idiv rdi"
      | Eq | Ne | Lt | Le ->
          emit "cmp rax, rdi";
          (match kind with
          | Eq -> emit "sete al"
          | Ne -> emit "setne al"
          | Lt -> emit "setl al"
          | Le -> emit "setle al"
          | _ -> Token.error input node.tok "invalid expression");
          emit "movzx rax, al"
      | _ -> Token.error input node.tok "invalid expression")

let rec gen_stmt self (input : string) (node : Node.t) =
  match node.kind with
  | Node.ExprStmt -> gen_expr self input (Option.get node.lhs)
  | Node.Return ->
      gen_expr self input (Option.get node.lhs);
      emit "jmp .L.return"
  | Node.Block body -> List.iter (gen_stmt self input) body
  | Node.If node ->
      let c = count self in
      gen_expr self input node.if_cond;
      emit "cmp rax, 0";
      emitf "je .L.else.%d" c;
      gen_stmt self input node.if_then_stmt;
      emitf "jmp .L.end.%d" c;
      Printf.printf ".L.else.%d:\n" c;
      Option.map (gen_stmt self input) node.if_else_stmt |> ignore;
      Printf.printf ".L.end.%d:\n" c
  | Node.For node ->
      let c = count self in
      Option.map (gen_stmt self input) node.for_init |> ignore;
      Printf.printf ".L.begin.%d:\n" c;
      Option.map
        (fun cond ->
          gen_expr self input cond;
          emit "cmp rax, 0";
          emitf "je .L.end.%d" c)
        node.for_cond
      |> ignore;
      gen_stmt self input node.for_body;
      Option.map (gen_expr self input) node.for_inc |> ignore;
      emitf "jmp .L.begin.%d" c;
      Printf.printf ".L.end.%d:\n" c
  | _ -> Token.error input node.tok "invalid statement"

let gen (input : string) (prog : Node.func) =
  let prog = ref prog in
  assign_lvar_offsets prog;
  let self = { depth = 0; counter = 1 } in
  emit "global main";
  print_endline "main:";
  emit "push rbp";
  emit "mov rbp, rsp";
  emitf "sub rsp, %d" !prog.func_stack_size;
  gen_stmt self input !prog.func_body;
  print_endline ".L.return:";
  emit "mov rsp, rbp";
  emit "pop rbp";
  emit "ret"
