type t = { locals : (string, Node.var ref) Hashtbl.t }

let find_var (self : t) (tok : Token.t) : Node.var ref option =
  Hashtbl.find_opt self.locals tok.text

let new_lvar (self : t) (name : string) : Node.var ref =
  let var = ref ({ name; offset = 0 } : Node.var) in
  Hashtbl.add self.locals name var;
  var

let lazy_value (default : unit -> 'a) (x : 'a option) : 'a =
  match x with Some x -> x | None -> default ()

(* primary: '(' expr ')' | num | ident *)
let rec primary (self : t) input rest (tok : Token.t list) =
  let tok = ref tok in
  match (List.hd !tok).text with
  | "(" ->
      let node = expr self input tok (List.tl !tok) in
      rest := Token.skip input !tok ")";
      node
  | _ -> (
      match (List.hd !tok).kind with
      | Token.Num value ->
          let node = Node.make_num value in
          rest := List.tl !tok;
          node
      | Token.Ident name ->
          let var =
            find_var self (List.hd !tok)
            |> lazy_value (fun () -> new_lvar self name)
          in
          let node = Node.make_var var in
          rest := List.tl !tok;
          node
      | _ -> Token.error input (List.hd !tok) "expected an expression")

(* unary: ( '+' | '-' ) unary
        | primary *)
and unary self input rest (tok : Token.t list) =
  match (List.hd tok).text with
  | "+" -> unary self input rest (List.tl tok)
  | "-" ->
      Node.make_binary Sub (Node.make_num 0)
        (unary self input rest (List.tl tok))
  | _ -> primary self input rest tok

(* mul: unary { '*' unary | '/' unary } *)
and mul self input rest tok =
  let tok = ref tok in
  let node = ref (unary self input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "*" ->
        let rhs = unary self input tok (List.tl !tok) in
        node := Node.make_binary Mul !node rhs
    | "/" ->
        let rhs = unary self input tok (List.tl !tok) in
        node := Node.make_binary Div !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* add: mul { '+' mul | '-' mul } *)
and add self input rest tok =
  let tok = ref tok in
  let node = ref (mul self input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "+" ->
        let rhs = mul self input tok (List.tl !tok) in
        node := Node.make_binary Add !node rhs
    | "-" ->
        let rhs = mul self input tok (List.tl !tok) in
        node := Node.make_binary Sub !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* relational: add { '<' add | '<=' add | '>' add | '>=' add } *)
and relational self input rest tok =
  let tok = ref tok in
  let node = ref (add self input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "<" ->
        let rhs = add self input tok (List.tl !tok) in
        node := Node.make_binary Lt !node rhs
    | "<=" ->
        let rhs = add self input tok (List.tl !tok) in
        node := Node.make_binary Le !node rhs
    | ">" ->
        let rhs = add self input tok (List.tl !tok) in
        node := Node.make_binary Lt rhs !node
    | ">=" ->
        let rhs = add self input tok (List.tl !tok) in
        node := Node.make_binary Le rhs !node
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

(* equality: relational { '==' relational | '!=' relational } *)
and equality self input rest tok =
  let tok = ref tok in
  let node = ref (relational self input tok !tok) in
  let looping = ref true in
  while !looping do
    match (List.hd !tok).text with
    | "==" ->
        let rhs = relational self input tok (List.tl !tok) in
        node := Node.make_binary Eq !node rhs
    | "!=" ->
        let rhs = relational self input tok (List.tl !tok) in
        node := Node.make_binary Ne !node rhs
    | _ ->
        rest := !tok;
        looping := false
  done;
  !node

and assign self input rest tok =
  let tok = ref tok in
  let node = ref (equality self input tok !tok) in
  (if Token.equal (List.hd !tok) "=" then
     let rhs = assign self input tok (List.tl !tok) in
     node := Node.make_binary Assign !node rhs);
  rest := !tok;
  !node

(* expr: assign *)
and expr self input rest tok = assign self input rest tok

(* expr-stmt: expr ';' *)
and expr_stmt self input rest tok =
  let tok = ref tok in
  let node = Node.make_unary ExprStmt (expr self input tok !tok) in
  rest := Token.skip input !tok ";";
  node

(* stmt: expr-stmt *)
and stmt input rest tok = expr_stmt input rest tok

let parse input (tokens : Token.t list) : Node.func =
  let tokens = ref tokens in
  let nodes = ref ([] : Node.t list) in
  let self : t = { locals = Hashtbl.create 1 } in
  while (List.hd !tokens).kind != Token.Eof do
    nodes := stmt self input tokens !tokens :: !nodes
  done;
  let last_tok = List.hd !tokens in
  if last_tok.kind != Token.Eof then Token.error input last_tok "extra token"
  else { body = !nodes |> List.rev; locals = self.locals; stack_size = 0 }
