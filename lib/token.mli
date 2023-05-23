type kind = Reserved | Ident of string | Num of int | Eof [@@deriving show]
type t = { kind : kind; text : string; pos : int } [@@deriving show]

val equal : t -> string -> bool
val skip : string -> t list -> string -> t list
val consume : t list ref -> t list -> string -> bool
val get_number : string -> t -> int
val make : kind -> string -> int -> t
val error : string -> t -> ('a, unit, string, 'b) format4 -> 'a
