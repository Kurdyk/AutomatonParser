{
open Parser
}

let layout = [ ' ' '\t']
let new_line = [ '\n' '\r' ]
let lettre = ['0'-'9' 'a'-'z' 'A'-'Z']

rule main = parse
  | layout		{ main lexbuf }
  | new_line   {let () = Lexing.new_line(lexbuf) in main lexbuf}
  | ')'			{ RPAREN }
  | '('			{ LPAREN }
  | "input symbols:" { INPUT_SYM }
  | "stack symbols:" { STACK_SYM }
  | "states:" { STATES }
  | "initial state:" { INIT_STATE }
  | "initial stack symbol:" { INIT_STACK }
  | "transitions:" { TRANSITION }
  | eof { EOF }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | lettre	{ LETTRE (Lexing.lexeme lexbuf) }
  | _			{ failwith "unexpected character" }
