type locals = (string, Node.var ref) Hashtbl.t

let find_var (locals : locals) (tok : Token.t) : Node.var ref option =
  Hashtbl.find_opt locals tok.text

let get_ident input (tok : Token.t) : string =
  match tok.kind with
  | Ident name -> name
  | _ -> Token.error input tok "expected an identifier"

let new_lvar (locals : locals) (var_ty : Type.t) (name : string) : Node.var ref
    =
  let var = ref ({ name; var_ty; offset = 0 } : Node.var) in
  Hashtbl.add locals name var;
  var

let lazy_value (default : unit -> 'a) (x : 'a option) : 'a =
  match x with Some x -> x | None -> default ()

(* funcall: ident '(' [ assign { ',' assign } ] ')' *)
let rec funcall (locals : locals) input rest (tok : Token.t list) =
  let start_tok = List.hd tok in
  let tok = ref (List.tl (List.tl tok)) in
  let cur = ref ([] : Node.t list) in
  let first = ref true in
  while not (Token.equal (List.hd !tok) ")") do
    if !first then first := false else tok := Token.skip input !tok ",";
    cur := assign locals input tok !tok :: !cur
  done;
  rest := Token.skip input !tok ")";
  Node.make start_tok
    (FunCall { funcall_name = start_tok.text; funcall_args = List.rev !cur })

(* primary: '(' expr ')' | num | ident [ func-args ] *)
and primary (locals : locals) input rest (tok : Token.t list) =
  let start_tok = List.hd tok in
  let tok = ref tok in
  match (List.hd !tok).text with
  | "(" ->
      let node = expr locals input tok (List.tl !tok) in
      rest := Token.skip input !tok ")";
      node
  | _ -> (
      match (List.hd !tok).kind with
      | Token.Num value ->
          let node = Node.make_num start_tok value in
          rest := List.tl !tok;
          node
      | Token.Ident _ ->
          if Token.equal (List.nth !tok 1) "(" then
            funcall locals input rest !tok
          else
            (* Variable *)
            let var =
              find_var locals (List.hd !tok)
              |> lazy_value (fun () ->
                     Token.error input (List.hd !tok) "undefined variable")
            in
            let node = Node.make_var start_tok var in
            rest := List.tl !tok;
            node
      | _ -> Token.error input (List.hd !tok) "expected an expression")

(* unary: ( '+' | '-' | '*' | '&' ) unary
        | primary *)
and unary locals input rest (tok : Token.t list) =
  let start_tok = List.hd tok in
  match start_tok.text with
  | "+" -> unary locals input rest (List.tl tok)
  | "-" ->
      Node.make_binary start_tok Sub
        (Node.make_num start_tok 0)
        (unary locals input rest (List.tl tok))
  | "&" ->
      Node.make_unary start_tok Addr (unary locals input rest (List.tl tok))
  | "*" ->
      Node.make_unary start_tok Deref (unary locals input rest (List.tl tok))
  | _ -> primary locals input rest tok

(* mul: unary { '*' unary | '/' unary } *)
and mul locals input rest tok =
  let start_tok = List.hd tok in
  let tok = ref tok in
  let node = ref (unary locals input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "*" ->
        let rhs = unary locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Mul !node rhs
    | "/" ->
        let rhs = unary locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Div !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* add: mul { '+' mul | '-' mul } *)
and add locals input rest tok =
  let start_tok = List.hd tok in
  let tok = ref tok in
  let node = ref (mul locals input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "+" ->
        let rhs = mul locals input tok (List.tl !tok) in
        node := Node.make_add input start_tok !node rhs
    | "-" ->
        let rhs = mul locals input tok (List.tl !tok) in
        node := Node.make_sub input start_tok !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* relational: add { '<' add | '<=' add | '>' add | '>=' add } *)
and relational locals input rest tok =
  let start_tok = List.hd tok in
  let tok = ref tok in
  let node = ref (add locals input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "<" ->
        let rhs = add locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Lt !node rhs
    | "<=" ->
        let rhs = add locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Le !node rhs
    | ">" ->
        let rhs = add locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Lt rhs !node
    | ">=" ->
        let rhs = add locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Le rhs !node
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* equality: relational { '==' relational | '!=' relational } *)
and equality locals input rest tok =
  let start_tok = List.hd tok in
  let tok = ref tok in
  let node = ref (relational locals input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "==" ->
        let rhs = relational locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Eq !node rhs
    | "!=" ->
        let rhs = relational locals input tok (List.tl !tok) in
        node := Node.make_binary start_tok Ne !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

and assign locals input rest tok =
  let start_tok = List.hd tok in
  let tok = ref tok in
  let node = ref (equality locals input tok !tok) in
  (if Token.equal (List.hd !tok) "=" then
     let rhs = assign locals input tok (List.tl !tok) in
     node := Node.make_binary start_tok Assign !node rhs);
  rest := !tok;
  !node

(* expr: assign *)
and expr locals input rest tok = assign locals input rest tok

(* expr-stmt: [ expr ] ';' *)
and expr_stmt locals input rest tok =
  let start_tok = List.hd tok in
  if Token.equal start_tok ";" then (
    rest := List.tl tok;
    Node.Block [] |> Node.make start_tok)
  else
    let tok = ref tok in
    let node =
      Node.make_unary start_tok ExprStmt (expr locals input tok !tok)
    in
    rest := Token.skip input !tok ";";
    node

(* stmt: 'return' expr ';'
       | 'if' '(' expr ')' stmt [ 'else' stmt ]
       | 'for' '(' expr-stmt [ expr ] ';' [ expr ] ')' stmt
       | '{' compound-stmt
       | expr-stmt *)
and stmt locals input rest (tok : Token.t list) =
  match (List.hd tok).text with
  | "return" ->
      let start_tok = List.hd tok in
      let tok = ref tok in
      let value = expr locals input tok (List.tl !tok) in
      let node = Node.make_unary start_tok Return value in
      rest := Token.skip input !tok ";";
      node
  | "if" ->
      let start_tok = List.hd tok in
      let tok = ref tok in
      tok := Token.skip input (List.tl !tok) "(";
      let if_cond = expr locals input tok !tok in
      tok := Token.skip input !tok ")";
      let if_then_stmt = stmt locals input tok !tok in
      let if_else_stmt =
        if Token.equal (List.hd !tok) "else" then
          Some (stmt locals input tok (List.tl !tok))
        else None
      in
      rest := !tok;
      Node.If { if_cond; if_then_stmt; if_else_stmt } |> Node.make start_tok
  | "for" ->
      let start_tok = List.hd tok in
      let tok = ref tok in
      tok := Token.skip input (List.tl !tok) "(";
      let for_init = Some (expr_stmt locals input tok !tok) in
      let for_cond =
        if not (Token.equal (List.hd !tok) ";") then
          Some (expr locals input tok !tok)
        else None
      in
      tok := Token.skip input !tok ";";
      let for_inc =
        if not (Token.equal (List.hd !tok) ")") then
          Some (expr locals input tok !tok)
        else None
      in
      tok := Token.skip input !tok ")";
      let for_body = stmt locals input rest !tok in
      Node.For { for_init; for_cond; for_inc; for_body } |> Node.make start_tok
  | "while" ->
      let start_tok = List.hd tok in
      let tok = ref tok in
      tok := Token.skip input (List.tl !tok) "(";
      let for_cond = expr locals input tok !tok in
      tok := Token.skip input !tok ")";
      let for_body = stmt locals input rest !tok in
      Node.For
        { for_init = None; for_cond = Some for_cond; for_inc = None; for_body }
      |> Node.make start_tok
  | "{" -> compound_stmt locals input rest (List.tl tok)
  | _ -> expr_stmt locals input rest tok

(* compound-stmt: { declaration | stmt } '}' *)
and compound_stmt locals input rest (tok : Token.t list) =
  let start_tok = List.hd tok in
  let tok = ref tok in
  let cur = ref ([] : Node.t list) in
  while not (String.equal (List.hd !tok).text "}") do
    let node =
      match (List.hd !tok).text with
      | "int" -> declaration locals input tok !tok
      | _ -> stmt locals input tok !tok
    in
    cur := (node |> Node.add_type) :: !cur
  done;
  rest := List.tl !tok;
  List.rev !cur |> fun x -> Node.Block x |> Node.make start_tok

(* declaration: typespec [ declarator [ '=' expr ] { ',' declarator [ '=' expr ] } ] ';' *)
and declaration locals input rest tok =
  let tok = ref tok in
  let basety = typespec input tok !tok in
  let cur = ref ([] : Node.t list) in
  let first = ref true in
  while not (Token.equal (List.hd !tok) ";") do
    if !first then first := false else tok := Token.skip input !tok ",";
    let ty = declarator input tok !tok basety in
    let var = get_ident input (Option.get ty.name) |> new_lvar locals ty in
    if Token.equal (List.hd !tok) "=" then
      let lhs = Node.make_var (List.hd !tok) var in
      let rhs = assign locals input tok (List.tl !tok) in
      let node = Node.make_binary (List.hd !tok) Assign lhs rhs in
      cur := Node.make_unary (List.hd !tok) ExprStmt node :: !cur
  done;
  let node =
    Node.make (List.hd !tok) (List.rev !cur |> fun x -> Node.Block x)
  in
  rest := List.tl !tok;
  node

(* typespec: 'int' *)
and typespec input rest tok : Type.t =
  rest := Token.skip input tok "int";
  Type.make Int

(* type-suffix: [ '(' func-params ] *)
and type_suffix input rest (tok : Token.t list) ty =
  match (List.hd tok).text with
  | "(" ->
      rest := Token.skip input (List.tl tok) ")";
      Type.func ty
  | _ ->
      rest := tok;
      ty

(* declarator: { '*' } ident type-suffix *)
and declarator input rest (tok : Token.t list) (ty : Type.t) =
  let tok = ref tok in
  let ty = ref ty in
  while Token.consume tok !tok "*" do
    ty := Type.ptr_to !ty
  done;
  let last_tok = List.hd !tok in
  match last_tok.kind with
  | Ident _ ->
      { !ty with name = Some last_tok } |> type_suffix input rest (List.tl !tok)
  | _ -> Token.error input last_tok "expected a variable name"

and func input rest tok : Node.func =
  let tok = ref tok in
  let ty = typespec input tok !tok |> declarator input tok !tok in
  let func_locals = Hashtbl.create 0 in
  let func_name = ty.name |> Option.get |> get_ident input in
  tok := Token.skip input !tok "{";
  let func_body = compound_stmt func_locals input rest !tok in
  { func_name; func_locals; func_body; func_stack_size = 0 }

let parse input (tokens : Token.t list) : Node.prog =
  let tokens = ref tokens in
  let nodes = ref ([] : Node.func list) in
  while (List.hd !tokens).kind != Token.Eof do
    nodes := func input tokens !tokens :: !nodes
  done;
  let last_tok = List.hd !tokens in
  if last_tok.kind != Token.Eof then Token.error input last_tok "extra token"
  else !nodes
