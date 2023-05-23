let () =
  let argc = Array.length Sys.argv in
  if argc != 2 then failwith "invalid number of arguments"
  else Chibic.Compiler.compile (Array.get Sys.argv 1) BatIO.stdout
