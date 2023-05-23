type kind = Int | Ptr of t | Func of func
and t = { kind : kind; name : Token.t option }
and func = { func_ty : t; func_params : t list }

let is_int (ty : t) : bool = match ty.kind with Int -> true | _ -> false
let is_ptr (ty : t) : bool = match ty.kind with Ptr _ -> true | _ -> false
let ptr_to ty = { ty with kind = Ptr ty }
let func ty = { ty with kind = Func { func_ty = ty; func_params = [] } }
let make kind = { kind; name = None }

let get_func ty =
  match ty.kind with Func f -> f | _ -> Error.error "not a func"
