let after s i = String.sub s i (String.length s - i)

let parse_int_opt (expr : string) : (int * string) option =
  Scanf.sscanf_opt expr "%u%[^\x00]" (fun x rest -> (x, rest))

let parse_int expr = Option.get (parse_int_opt expr)
let is_blank c = match c with ' ' | '\r' | '\n' | '\t' -> true | _ -> false
let is_digit c = match c with '0' .. '9' -> true | _ -> false

let rec orp (preds : ('a -> bool) list) value =
  match preds with
  | [] -> false
  | [ p ] -> p value
  | p :: ps -> p value || orp ps value

let starts_with_any ps =
  List.map (fun p -> String.starts_with ~prefix:p) ps |> orp

let eq_any ss = List.map String.equal ss |> orp

let is_ident1 c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_ident2 c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false

let is_keyword (tok : Token.t) =
  eq_any [ "return"; "if"; "else"; "for"; "while"; "int"; "sizeof" ] tok.text

let convert_keywords (toks : Token.t list) : Token.t list =
  List.map
    (fun tok -> if is_keyword tok then { tok with kind = Reserved } else tok)
    toks

let tokenize text =
  let cur = ref ([] : Token.t list) in
  let p = ref 0 in
  while !p < String.length text do
    if after text !p |> starts_with_any [ "=="; "!="; "<="; ">=" ] then (
      cur := Token.make Reserved (String.sub text !p 2) !p :: !cur;
      p := !p + 2)
    else
      match String.get text !p with
      | c when is_blank c -> p := !p + 1
      | c when is_digit c ->
          let value, rest = parse_int (after text !p) in
          let len = String.length text - !p - String.length rest in
          cur := Token.make (Num value) (String.sub text !p len) !p :: !cur;
          p := !p + len
      | c when is_ident1 c ->
          let q = !p in
          p := !p + 1;
          while is_ident2 (String.get text !p) do
            p := !p + 1
          done;
          let len = !p - q in
          let id = String.sub text q len in
          cur := Token.make (Ident id) id q :: !cur
      | '+' | '-' | '*' | '/' | '(' | ')' | '{' | '}' | '[' | ']' | '>' | '<'
      | ';' | '=' | '&' | ',' ->
          cur := Token.make Reserved (String.sub text !p 1) !p :: !cur;
          p := !p + 1
      | _ -> Error.error_at text !p "invalid token"
  done;
  cur := Token.make Eof "" !p :: !cur;
  List.rev !cur |> convert_keywords
