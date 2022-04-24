let lexbuf = Lexing.from_channel stdin 

let automaton = Parser.automate Lexer.main lexbuf 

let _ = Printf.printf "Parse:\n%s\n" (Ast.as_string_auto automaton)
let test = Execution.execute automaton [Symbol(Lettre("a")); Symbol(Lettre("c")); Symbol(Lettre("b"))]
