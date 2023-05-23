type t = Int | Ptr of t

let is_int (ty : t) : bool = match ty with Int -> true | _ -> false
let is_ptr (ty : t) : bool = match ty with Ptr _ -> true | _ -> false
