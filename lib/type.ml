type kind = Int | Ptr of t | Func of func | Array of t * int [@@deriving show]
and t = { kind : kind; sizeof : int; name : Token.t option } [@@deriving show]
and func = { func_ty : t; func_params : t list } [@@deriving show]

let is_int (ty : t) : bool = match ty.kind with Int -> true | _ -> false
let is_ptr (ty : t) : bool = match ty.kind with Ptr _ -> true | _ -> false
let ptr_to ty = { ty with kind = Ptr ty }

let array_of ty len =
  { ty with kind = Array (ty, len); sizeof = ty.sizeof * len }

let func ty = { ty with kind = Func { func_ty = ty; func_params = [] } }
let make kind sizeof = { kind; sizeof; name = None }
let int : t = make Int 8

let get_func ty =
  match ty.kind with Func f -> f | _ -> Error.error "not a func"

let baseof ty =
  match ty.kind with
  | Array (base, _) -> Some base
  | Ptr base -> Some base
  | _ -> None
