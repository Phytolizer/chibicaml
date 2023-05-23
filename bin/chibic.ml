open Chibic

let compile (text : string) =
  Lexer.tokenize text |> Parser.parse text |> Codegen.gen text

let () =
  let argc = Array.length Sys.argv in
  if argc != 2 then failwith "invalid number of arguments"
  else compile (Array.get Sys.argv 1)
