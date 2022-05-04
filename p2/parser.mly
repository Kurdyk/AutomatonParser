%{
open Ast
%}

%token LPAREN RPAREN INPUT_SYM STACK_SYM STATES INIT_STATE INIT_STACK TRANSITION COMMA SEMICOLON EOF PROGRAM COLON C_NEXT C_STATE C_TOP BEGIN END POP PUSH REJECT CHANGE
%token<string> LETTRE

%start<Ast.automate2> automate

%%

  
automate:
    d=declaration p=program EOF { Automate2 (d, p) }

declaration:
    input_s=inputsymbols stack_s=stacksymbols s=states init_state=initialstate init_stack=initialstack {Declaration (input_s, stack_s, s, init_state, init_stack)}

inputsymbols:
    INPUT_SYM l=suitelettres_nonvide  {l}

stacksymbols:
    STACK_SYM l=suitelettres_nonvide  {l}

states:
    STATES l=suitelettres_nonvide  {l}

initialstate:
    INIT_STATE l=LETTRE {Symbol(Lettre(l))}

initialstack:
    INIT_STACK l=LETTRE {Symbol(Lettre(l))}

suitelettres_nonvide:
    | l=LETTRE {[Symbol(Lettre(l))]}
    | l=LETTRE COMMA suite=suitelettres_nonvide {[Symbol(Lettre(l))] @ suite}


program:
    | PROGRAM b=block {b}

block:
    | C_STATE l=state_possibility { State(l) }
    | C_TOP l=top_possibility { Top(l) }
    | C_NEXT l=next_possibility { Next(l) }


state_possibility:
    | statesym=LETTRE COLON BEGIN b=block END { [(Symbol(Lettre(statesym)), b)] }
    | statesym=LETTRE COLON BEGIN b=block END suite=state_possibility { (Symbol(Lettre(statesym)), b) :: suite }
    | statesym=LETTRE COLON i=instruction { [(Symbol(Lettre(statesym)), Instruction(i))] }
    | statesym=LETTRE COLON i=instruction suite=state_possibility { (Symbol(Lettre(statesym)), Instruction(i)) :: suite }

top_possibility:
    | stacksym=LETTRE COLON BEGIN b=block END { [(Symbol(Lettre(stacksym)), b)] }
    | stacksym=LETTRE COLON BEGIN b=block END suite=top_possibility { (Symbol(Lettre(stacksym)), b) :: suite }
    | stacksym=LETTRE COLON i=instruction { [(Symbol(Lettre(stacksym)), Instruction(i))] }
    | stacksym=LETTRE COLON i=instruction suite=top_possibility { (Symbol(Lettre(stacksym)), Instruction(i)) :: suite }

next_possibility:
    | inputsym=LETTRE COLON i=instruction { [(Symbol(Lettre(inputsym)), Instruction(i))] }
    | inputsym=LETTRE COLON i=instruction suite=next_possibility { (Symbol(Lettre(inputsym)), Instruction(i)) :: suite } 

instruction:
    | POP {Pop}
    | REJECT {Reject}
    | PUSH l=LETTRE {Push(Symbol(Lettre(l)))}
    | CHANGE l=LETTRE {Change(Symbol(Lettre(l)))}

transitions:
    TRANSITION t=translist {t}

translist:
    | {[]}
    | t=transition suite=translist {[t] @ suite}

transition:
    LPAREN l1=LETTRE COMMA l2=lettre_ou_vide COMMA l3=LETTRE COMMA l4=LETTRE COMMA s=stack RPAREN  {Transition(Symbol(Lettre(l1)), Symbol(l2), Symbol(Lettre(l3)), Symbol(Lettre(l4)), s)}

lettre_ou_vide:
    | {Epsilon}
    | l=LETTRE {Lettre(l)}

stack:
    | {Empty}
    | s=nonemptystack {s}

nonemptystack:
    | l=LETTRE {Empilage(Symbol(Lettre(l)))}
    | l1=LETTRE SEMICOLON rempl=nonemptystack {Remplace(Symbol(Lettre(l1)), rempl)}

