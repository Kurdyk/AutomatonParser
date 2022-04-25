(*Type declaration*)
type lettre_ou_vide = 
    | Lettre of string
    | Number of int
    | Epsilon

type lettre_non_vide =
    | Lettre of string
    | Number of int

type symbol = Symbol of lettre_ou_vide

type inputsymbol = symbol

type inputsymbols = inputsymbol list

type stacksymbol = symbol

type stacksymbols = stacksymbol list

type state = symbol

type states = state list

type declaration = Declaration of inputsymbols * stacksymbols * states * state * stacksymbol

type remplacement = 
    | Empty
    | Empilage of stacksymbol
    | Remplace of stacksymbol * remplacement

type transition = Transition of state * inputsymbol * stacksymbol * state * remplacement

type automate = Automate of declaration * transition list

(*To string*)
let as_string_lettre lettre = match lettre with
  | Epsilon -> "esp"
  | Lettre x -> x
  | Number x -> string_of_int x

let as_string_sym sym = match sym with
  | Symbol x -> as_string_lettre x 

let rec as_string_sym_list syml = match syml with
| [] -> ""
| x::[] -> as_string_sym x
| x::xs -> as_string_sym x ^ ", " ^ as_string_sym_list xs

let rec  as_string_rempl rempl = match rempl with
| Empty -> "esp"
| Empilage x -> as_string_sym x
| Remplace (x, xl) -> as_string_sym x ^ " ; " ^ as_string_rempl xl

let as_string_transi transi = match transi with
  | Transition(si, s1, s2, sf ,r) -> "(" ^  as_string_sym si ^ ", " ^ as_string_sym s1 ^ ", " ^ as_string_sym s2 ^ ", " ^ as_string_sym sf ^ ", " ^ as_string_rempl r ^ ")\n"

let rec as_string_transi_list transis = match transis with
  | [] -> failwith("shouldn't happen")
  | t::[] -> as_string_transi t
  | t::ts -> as_string_transi t ^ as_string_transi_list ts

let as_string_decl decl = match decl with
  | Declaration(x, y, sl, s, s1) -> "input symbols: " ^ as_string_sym_list x ^ "\nstack_symbols: " ^ as_string_sym_list y ^ "\nstates: " ^ 
                                    as_string_sym_list sl ^ "\ninitial_state: " ^ as_string_sym s ^ "\ninitial_stack: " ^ as_string_sym s1 ^ "\n"

let rec as_string_auto (auto:automate) = match auto with
  | Automate(decl, transi) -> "\027[32mDeclarations\027[0m:\n" ^ as_string_decl decl 
  ^ "\r\n\027[32mTransitions\027[0m:\n" ^ as_string_transi_list transi ^ "\n"


(*Getteur*)
let get_declaration (auto:automate) :declaration = match auto with
| Automate(decl, transi) -> decl

let get_input_symbols (decl:declaration) :inputsymbols = match decl with
  | Declaration(s, _, _, _, _) -> s

let get_stack_symbols (decl:declaration) :stacksymbols = match decl with
| Declaration(_, s, _, _, _) -> s

let get_state_symbols (decl:declaration) :states = match decl with
| Declaration(_, _, s, _, _) -> s

let get_init_stack (decl:declaration) :inputsymbol = match decl with
| Declaration(_, _, _, _, s) -> s

let get_init_state (decl:declaration) :state = match decl with
| Declaration(_, _, _, state, _) -> state

let get_transitions (auto:automate) :transition list = match auto with
| Automate(decl, transis) -> transis

let get_necessary_top (transi:transition) :stacksymbol = match transi with 
  | Transition(_, _, top, _, _) -> top

let get_necessary_state (transi:transition) :state = match transi with 
  | Transition(state, _, _, _, _) -> state

let get_necessary_input (transi:transition) :inputsymbol = match transi with 
  | Transition(_, input_c, _ , _, _) -> input_c

let get_remplacement (transi:transition) :remplacement = match transi with
| Transition(_, _, _, _, r) -> r

let get_new_state (transi:transition) :state = match transi with
| Transition(_, _, _, state, _) -> state

