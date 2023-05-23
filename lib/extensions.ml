module Option = struct
  include Option

  let lazy_value (default : 'a lazy_t) (x : 'a option) : 'a =
    match x with Some x -> x | None -> Lazy.force default

  let or_lazy (orelse : 'a option lazy_t) (x : 'a option) : 'a option =
    match x with Some _ -> x | None -> Lazy.force orelse
end
