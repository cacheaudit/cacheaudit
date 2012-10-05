


let _ = Arg.parse [] (fun f -> Parser.main Lex.line (Lexing.from_channel (open_in f))) "test string"
