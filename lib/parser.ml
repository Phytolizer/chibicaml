(* primary: '(' expr ')' | num | ident *)
let rec primary input rest (tok : Token.t list) =
  let tok = ref tok in
  match (List.hd !tok).text with
  | "(" ->
      let node = expr input tok (List.tl !tok) in
      rest := Token.skip input !tok ")";
      node
  | _ -> (
      match (List.hd !tok).kind with
      | Token.Num value ->
          let node = Node.make_num value in
          rest := List.tl !tok;
          node
      | Token.Ident name ->
          let node = Node.make_var name in
          rest := List.tl !tok;
          node
      | _ -> Token.error input (List.hd !tok) "expected an expression")

(* unary: ( '+' | '-' ) unary
        | primary *)
and unary input rest (tok : Token.t list) =
  match (List.hd tok).text with
  | "+" -> unary input rest (List.tl tok)
  | "-" ->
      Node.make_binary Sub (Node.make_num 0) (unary input rest (List.tl tok))
  | _ -> primary input rest tok

(* mul: unary { '*' unary | '/' unary } *)
and mul input rest tok =
  let tok = ref tok in
  let node = ref (unary input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "*" ->
        let rhs = unary input tok (List.tl !tok) in
        node := Node.make_binary Mul !node rhs
    | "/" ->
        let rhs = unary input tok (List.tl !tok) in
        node := Node.make_binary Div !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* add: mul { '+' mul | '-' mul } *)
and add input rest tok =
  let tok = ref tok in
  let node = ref (mul input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "+" ->
        let rhs = mul input tok (List.tl !tok) in
        node := Node.make_binary Add !node rhs
    | "-" ->
        let rhs = mul input tok (List.tl !tok) in
        node := Node.make_binary Sub !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* relational: add { '<' add | '<=' add | '>' add | '>=' add } *)
and relational input rest tok =
  let tok = ref tok in
  let node = ref (add input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "<" ->
        let rhs = add input tok (List.tl !tok) in
        node := Node.make_binary Lt !node rhs
    | "<=" ->
        let rhs = add input tok (List.tl !tok) in
        node := Node.make_binary Le !node rhs
    | ">" ->
        let rhs = add input tok (List.tl !tok) in
        node := Node.make_binary Lt rhs !node
    | ">=" ->
        let rhs = add input tok (List.tl !tok) in
        node := Node.make_binary Le rhs !node
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* equality: relational { '==' relational | '!=' relational } *)
and equality input rest tok =
  let tok = ref tok in
  let node = ref (relational input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "==" ->
        let rhs = relational input tok (List.tl !tok) in
        node := Node.make_binary Eq !node rhs
    | "!=" ->
        let rhs = relational input tok (List.tl !tok) in
        node := Node.make_binary Ne !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

and assign input rest tok =
  let tok = ref tok in
  let node = ref (equality input tok !tok) in
  (if Token.equal (List.hd !tok) "=" then
     let rhs = assign input tok (List.tl !tok) in
     node := Node.make_binary Assign !node rhs);
  rest := !tok;
  !node

(* expr: assign *)
and expr input rest tok = assign input rest tok

(* expr-stmt: expr ';' *)
and expr_stmt input rest tok =
  let tok = ref tok in
  let node = Node.make_unary ExprStmt (expr input tok !tok) in
  rest := Token.skip input !tok ";";
  node

(* stmt: expr-stmt *)
and stmt input rest tok = expr_stmt input rest tok

let parse input (tokens : Token.t list) =
  let tokens = ref tokens in
  let nodes = ref ([] : Node.t list) in
  while (List.hd !tokens).kind != Token.Eof do
    nodes := stmt input tokens !tokens :: !nodes
  done;
  let last_tok = List.hd !tokens in
  if last_tok.kind != Token.Eof then Token.error input last_tok "extra token"
  else !nodes |> List.rev |> fun x -> Prog x |> Node.make
