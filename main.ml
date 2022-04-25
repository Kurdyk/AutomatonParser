let lexbuf = Lexing.from_channel stdin 

  let automaton = 
    try
      Parser.automate Lexer.main lexbuf 
  with _ ->
    let position = lexbuf.lex_curr_p in 
    let error_msg = Printf.sprintf "Parsing failure at line %d, char %d" position.pos_lnum (position.pos_cnum - position.pos_bol)
    in failwith error_msg

let _ = Printf.printf "\027[31mParse:\n\027[0m%s" (Ast.as_string_auto automaton)

let _ = Printf.printf "\027[31mAnalyse of the automaton:\027[0m\n"
let _ = Analyser.analyse_init_stack automaton
let _ = Analyser.analyse_init_state automaton
let _ = Analyser.analyse_transistions_information automaton
let _ = Analyser.analyse_determinization automaton
let _ = Printf.printf "\n\027[31mEnter your word:\027[0m\n"

let test = Execution.execute automaton [Symbol(Lettre("a")); Symbol(Lettre("c"));  Symbol(Lettre("a"))]
