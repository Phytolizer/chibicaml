type kind = Reserved | Ident of char | Num of int | Eof
type t = { kind : kind; text : string; pos : int }

val equal : t -> string -> bool
val skip : string -> t list -> string -> t list
val get_number : string -> t -> int
val make : kind -> string -> int -> t
val error : string -> t -> ('a, unit, string, 'b) format4 -> 'a
