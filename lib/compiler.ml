open BatIO

let compile (text : string) (out : 'a output) =
  Lexer.tokenize text |> Parser.parse text |> Codegen.gen out text
