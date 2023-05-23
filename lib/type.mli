type t = Int | Ptr of t

val is_int : t -> bool
val is_ptr : t -> bool
