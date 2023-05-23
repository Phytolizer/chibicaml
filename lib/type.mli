type kind = Int | Ptr of t
and t = { kind : kind; name : Token.t option }

val is_int : t -> bool
val is_ptr : t -> bool

val ptr_to : t -> t

val make : kind -> t
