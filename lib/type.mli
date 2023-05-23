type kind = Int | Ptr of t | Func of func
and t = { kind : kind; name : Token.t option }
and func = { func_ty : t; func_params : t list }

val is_int : t -> bool
val is_ptr : t -> bool
val ptr_to : t -> t
val func : t -> t
val make : kind -> t
val get_func : t -> func
