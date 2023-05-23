type t = { mutable depth : int; mutable counter : int; current_fn : Node.func }

let argreg : string Seq.t =
  List.to_seq [ "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" ]

let emit text =
  print_char '\t';
  print_endline text

let emitf text = Printf.ksprintf (fun s -> Printf.printf "\t%s\n" s) text

let push (self : t) =
  emit "push rax";
  self.depth <- self.depth + 1

let pop (self : t) arg =
  emitf "pop %s" arg;
  self.depth <- self.depth - 1

let count (self : t) =
  let result = self.counter in
  self.counter <- self.counter + 1;
  result

let align_to (n : int) (align : int) : int = (n + align - 1) / align * align

let assign_lvar_offsets (f : Node.func) =
  let offset = ref 0 in
  Hashtbl.iter
    (fun _ (var : Node.var ref) ->
      offset := !offset + 8;
      var := { !var with offset = !offset })
    f.func_locals;
  { f with func_stack_size = align_to !offset 16 }

let rec gen_addr (self : t) (input : string) (node : Node.t) =
  match node.kind with
  | Node.Var var ->
      let offset = !var.offset in
      emitf "lea rax, [rbp - %d]" offset
  | Node.Deref -> gen_expr self input (Option.get node.lhs)
  | _ -> Token.error input node.tok "not an lvalue"

and gen_expr (self : t) (input : string) (node : Node.t) =
  match node.kind with
  | Num value -> emitf "mov rax, %d" value
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
  | FunCall node ->
      let nargs =
        List.length
          (List.map
             (fun arg ->
               gen_expr self input arg;
               push self)
             node.funcall_args)
      in
      List.iter (pop self) (Seq.take nargs argreg |> List.of_seq |> List.rev);
      emit "mov rax, 0";
      emitf "extern %s" node.funcall_name;
      emitf "call %s" node.funcall_name
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
      emitf "jmp .L.return.%s" self.current_fn.func_name
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

let gen_fn (input : string) (f : Node.func) =
  let f = assign_lvar_offsets f in
  let self = { depth = 0; counter = 1; current_fn = f } in
  emitf "global %s" f.func_name;
  Printf.printf "%s:\n" f.func_name;
  emit "push rbp";
  emit "mov rbp, rsp";
  emitf "sub rsp, %d" f.func_stack_size;
  gen_stmt self input f.func_body;
  Printf.printf ".L.return.%s:\n" f.func_name;
  emit "mov rsp, rbp";
  emit "pop rbp";
  emit "ret"

let gen (input : string) (prog : Node.prog) = List.iter (gen_fn input) prog
