{
open Parser
}

let layout = [ ' ' '\t' '\n']
let lettre = ['0'-'9' 'a'-'z' 'A'-'Z']

rule main = parse
  | layout		{ main lexbuf }
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
