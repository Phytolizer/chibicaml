type kind = Int | Ptr of t | Func of t
and t = { kind : kind; name : Token.t option }

let is_int (ty : t) : bool = match ty.kind with Int -> true | _ -> false
let is_ptr (ty : t) : bool = match ty.kind with Ptr _ -> true | _ -> false
let ptr_to ty = { ty with kind = Ptr ty }
let func ty = { ty with kind = Func ty }
let make kind = { kind; name = None }
