let ic = open_in Sys.argv.(1)

let lexbuf = Lexing.from_channel ic

let automaton = 
  try
    Parser.automate Lexer.main lexbuf 
with _ ->
  let position = lexbuf.lex_curr_p in 
  let error_msg = Printf.sprintf "Parsing failure at line %d, char %d" position.pos_lnum (position.pos_cnum - position.pos_bol)
  in failwith error_msg

;;

Printf.printf "\027[31mParse:\n\027[0m%s" (Ast.as_string_auto automaton) ;
Printf.printf "\027[31mAnalyse of the automaton:\027[0m\n" ;
Analyser.analyse_init_stack automaton ;
Analyser.analyse_init_state automaton ;
Analyser.analyse_transistions_information automaton ;
Analyser.analyse_determinization automaton ;
Printf.printf "\n\027[31mEnter your word:\027[0m\n" ;
flush stdout ;

Execution.execute automaton (Ast.to_sym (input_line stdin) []) ;