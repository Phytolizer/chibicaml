type kind = Int | Ptr of t | Func of func | Array of t * int [@@deriving show]
and t = { kind : kind; sizeof : int; name : Token.t option } [@@deriving show]
and func = { func_ty : t; func_params : t list } [@@deriving show]

val int : t
val is_int : t -> bool
val is_ptr : t -> bool
val ptr_to : t -> t
val func : t -> t
val array_of : t -> int -> t
val make : kind -> int -> t
val get_func : t -> func
val baseof : t -> t option
