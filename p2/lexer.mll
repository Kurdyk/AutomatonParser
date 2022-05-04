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
  | "initial stack:" { INIT_STACK }
  | "transitions:" { TRANSITION }
  | eof { EOF }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | "program:" { PROGRAM }
  | ":" { COLON }
  | "case next of" { C_NEXT }
  | "case top of" { C_TOP }
  | "case state of" { C_STATE }
  | "begin" { BEGIN }
  | "end" { END }
  | "pop" { POP }
  | "push" { PUSH }
  | "reject" { REJECT }
  | "change" { CHANGE }
  | lettre	{ LETTRE (Lexing.lexeme lexbuf) }
  | _			{ failwith "unexpected character" }