let error s = Printf.ksprintf failwith s

let error_at (input : string) (loc : int) s =
  Printf.ksprintf
    (fun s ->
      prerr_endline input;
      for _ = 0 to loc - 1 do
        prerr_char ' '
      done;
      Printf.eprintf "^ %s\n" s;
      failwith "ERROR")
    s
