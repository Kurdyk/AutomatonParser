%{
open Ast
%}

%token LPAREN RPAREN INPUT_SYM STACK_SYM STATES INIT_STATE INIT_STACK TRANSITION COMMA SEMICOLON EOF
%token<string> LETTRE

%start<Ast.automate> automate

%%

  
automate:
    d=declaration t=transitions EOF { Automate (d, t) }

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

