type kind = Reserved | Ident of char | Num of int | Eof
type t = { kind : kind; text : string; pos : int }

let equal (tok : t) : string -> bool = String.equal tok.text
let error (input : string) (tok : t) = Error.error_at input tok.pos

let skip (input : string) (toks : t list) (s : string) : t list =
  let tok = List.hd toks in
  if not (equal tok s) then error input tok "expected '%s'" s else List.tl toks

let get_number (input : string) (tok : t) =
  match tok.kind with Num n -> n | _ -> error input tok "expected a number"

let make (kind : kind) (text : string) (pos : int) : t = { kind; text; pos }
