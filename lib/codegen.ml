open BatIO

type t = { mutable depth : int; mutable counter : int; current_fn : Node.func }

let argreg : string Seq.t =
  List.to_seq [ "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" ]

let emit out text =
  write out '\t';
  nwrite out text;
  write out '\n'

let emitf out = BatPrintf.ksprintf (fun s -> BatPrintf.fprintf out "\t%s\n" s)

let push out (self : t) =
  emit out "push rax";
  self.depth <- self.depth + 1

let pop out (self : t) arg =
  emitf out "pop %s" arg;
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

let rec gen_addr out (self : t) (input : string) (node : Node.t) =
  match node.kind with
  | Node.Var var ->
      let offset = !var.offset in
      emitf out "lea rax, [rbp - %d]" offset
  | Node.Deref -> gen_expr out self input (Option.get node.lhs)
  | _ -> Token.error input node.tok "not an lvalue"

and gen_expr out (self : t) (input : string) (node : Node.t) =
  match node.kind with
  | Num value -> emitf out "mov rax, %d" value
  | Var _ ->
      gen_addr out self input node;
      emit out "mov rax, [rax]"
  | Deref ->
      gen_expr out self input (Option.get node.lhs);
      emit out "mov rax, [rax]"
  | Addr -> gen_addr out self input (Option.get node.lhs)
  | Assign ->
      gen_addr out self input (Option.get node.lhs);
      push out self;
      gen_expr out self input (Option.get node.rhs);
      pop out self "rdi";
      emit out "mov [rdi], rax"
  | FunCall node ->
      let nargs =
        List.length
          (List.map
             (fun arg ->
               gen_expr out self input arg;
               push out self)
             node.funcall_args)
      in
      List.iter (pop out self) (Seq.take nargs argreg |> List.of_seq |> List.rev);
      emit out "mov rax, 0";
      emitf out "extern %s" node.funcall_name;
      emitf out "call %s" node.funcall_name
  | kind -> (
      gen_expr out self input (Option.get node.rhs);
      push out self;
      gen_expr out self input (Option.get node.lhs);
      pop out self "rdi";
      match kind with
      | Add -> emit out "add rax, rdi"
      | Sub -> emit out "sub rax, rdi"
      | Mul -> emit out "imul rax, rdi"
      | Div ->
          emit out "cqo";
          emit out "idiv rdi"
      | Eq | Ne | Lt | Le ->
          emit out "cmp rax, rdi";
          (match kind with
          | Eq -> emit out "sete al"
          | Ne -> emit out "setne al"
          | Lt -> emit out "setl al"
          | Le -> emit out "setle al"
          | _ -> Token.error input node.tok "invalid expression");
          emit out "movzx rax, al"
      | _ -> Token.error input node.tok "invalid expression")

let rec gen_stmt out self (input : string) (node : Node.t) =
  match node.kind with
  | Node.ExprStmt -> gen_expr out self input (Option.get node.lhs)
  | Node.Return ->
      gen_expr out self input (Option.get node.lhs);
      emitf out "jmp .L.return.%s" self.current_fn.func_name
  | Node.Block body -> List.iter (gen_stmt out self input) body
  | Node.If node ->
      let c = count self in
      gen_expr out self input node.if_cond;
      emit out "cmp rax, 0";
      emitf out "je .L.else.%d" c;
      gen_stmt out self input node.if_then_stmt;
      emitf out "jmp .L.end.%d" c;
      BatPrintf.fprintf out ".L.else.%d:\n" c;
      Option.map (gen_stmt out self input) node.if_else_stmt |> ignore;
      BatPrintf.fprintf out ".L.end.%d:\n" c
  | Node.For node ->
      let c = count self in
      Option.map (gen_stmt out self input) node.for_init |> ignore;
      BatPrintf.fprintf out ".L.begin.%d:\n" c;
      Option.map
        (fun cond ->
          gen_expr out self input cond;
          emit out "cmp rax, 0";
          emitf out "je .L.end.%d" c)
        node.for_cond
      |> ignore;
      gen_stmt out self input node.for_body;
      Option.map (gen_expr out self input) node.for_inc |> ignore;
      emitf out "jmp .L.begin.%d" c;
      BatPrintf.fprintf out ".L.end.%d:\n" c
  | _ -> Token.error input node.tok "invalid statement"

let gen_fn out (input : string) (f : Node.func) =
  let f = assign_lvar_offsets f in
  let self = { depth = 0; counter = 1; current_fn = f } in
  emitf out "global %s" f.func_name;
  BatPrintf.fprintf out "%s:\n" f.func_name;
  emit out "push rbp";
  emit out "mov rbp, rsp";
  emitf out "sub rsp, %d" f.func_stack_size;
  gen_stmt out self input f.func_body;
  BatPrintf.fprintf out ".L.return.%s:\n" f.func_name;
  emit out "mov rsp, rbp";
  emit out "pop rbp";
  emit out "ret"

let gen out (input : string) (prog : Node.prog) =
  List.iter (gen_fn out input) prog
