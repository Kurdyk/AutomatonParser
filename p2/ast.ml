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

type instruction = 
    | Pop
    | Push of stacksymbol
    | Reject
    | Change of state


type block =
    | Next of (inputsymbol * block) list
    | Top of (stacksymbol * block) list
    | State of (state * block) list
    | Instruction of instruction


type declaration = Declaration of inputsymbols * stacksymbols * states * state * stacksymbol

type remplacement = 
    | Empty
    | Empilage of stacksymbol
    | Remplace of stacksymbol * remplacement

type transition = Transition of state * inputsymbol * stacksymbol * state * remplacement

type automate = Automate of declaration * transition list
type automate2 = Automate2 of declaration * block



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

(*From string*)

let rec to_sym (str:string) (acc:inputsymbols) :inputsymbols = match str with
    | "" -> List.rev acc
    | s -> to_sym (String.sub s 1 ((String.length s) - 1)) (Symbol(Lettre(String.make 1 (String.get s 0)))::acc)


let rec remplace_add r z = match r with
  | Empty -> Empilage(z)
  | Empilage(ssym) -> Remplace(ssym, Empilage(z))
  | Remplace(ssym, rempsym) -> Remplace(ssym, (remplace_add rempsym z))



let convert_v2_v1 (auto:automate2):automate = 
  let rec build_transitions (decl:declaration) (bl:block) : transition list= 
    let rec build_default_transitions (state_start:state) (inputsym:inputsymbol) (stacksym:stacksymbol) (state_end:state) (replac:remplacement) = 
      let rec build_no_stacksym l = match l with
        | [] -> []
        | h :: q -> let rep = match replac with | Empty -> Empty 
                                                | Empilage(Symbol(Epsilon)) -> Empilage(h) 
                                                | Empilage(_) -> replac
                                                | Remplace(Symbol(Epsilon), a) -> Remplace(h, a)
                                                | Remplace(_, _) -> replac
                    in Transition(state_start, inputsym, h, state_end, rep) :: build_no_stacksym q
  in let rec build_no_statesym l = match l with
        | [] -> []
        | h :: q -> Transition(h, inputsym, stacksym, h, replac) :: build_no_statesym q
  in let rec build_no_statesym_stacksym l1 l2 = 
    let rec b_no_state_stack_interne s1 l2 = match l2 with
        | [] -> []
        | h :: q -> let rep = match replac with | Empty -> Empty 
                                                | Empilage(Symbol(Epsilon)) -> Empilage(h) 
                                                | Empilage(_) -> replac
                                                | Remplace(Symbol(Epsilon), a) -> Remplace(h, a)
                                                | Remplace(_, _) -> replac
                    in Transition(s1, inputsym, h, s1, rep) :: b_no_state_stack_interne s1 q
    in match l1 with 
      | [] -> []
      | i :: j -> (b_no_state_stack_interne i l2) @ (build_no_statesym_stacksym j l2)
  in let decl = match auto with Automate2(decl, bl) -> decl in
    match (state_start, stacksym) with 
      | (Symbol(Epsilon), Symbol(Epsilon)) -> build_no_statesym_stacksym (get_state_symbols decl) (get_stack_symbols decl)
      | (Symbol(Epsilon), _) -> build_no_statesym (get_state_symbols decl)
      | (_, Symbol(Epsilon)) -> build_no_stacksym (get_stack_symbols decl)
      | (_, _) -> [Transition(state_start, inputsym, stacksym, state_end, replac)]
  in let rec override_outcome trlist out = match trlist with
    | [] -> []
    | Transition(a, b, c, d, e) :: q -> Transition(a, b, c, out, e) :: override_outcome q out
  in let rec disassemble bl l (state_start:state) (inputsym:inputsymbol) (stacksym:stacksymbol) (state_end:state) (replac:remplacement) : transition list= match (bl, l) with
      | (_, []) -> []
      | (State(_), (s, b) :: q) -> (match b with
                | Next(blist) | Top(blist) | State(blist) -> (disassemble b blist s inputsym stacksym s replac) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                | Instruction(i) -> match i with
                        | Pop -> (build_default_transitions s inputsym stacksym s Empty) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                        | Reject -> failwith "The word is rejected!"
                        | Push(z) -> (build_default_transitions s inputsym stacksym s (remplace_add (Empilage(stacksym)) z)) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                        | Change(e) -> override_outcome (build_default_transitions s inputsym stacksym e (Empilage(stacksym))) e @ (disassemble bl q state_start inputsym stacksym state_end replac))
      | (Top(_), (s, b) :: q) -> (match b with
                | Next(blist) | Top(blist) | State(blist) -> (disassemble b blist state_start inputsym s state_end replac) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                | Instruction(i) -> match i with 
                        | Pop -> (build_default_transitions state_start inputsym s state_end Empty) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                        | Reject -> failwith "The word is rejected!"
                        | Push(z) -> (build_default_transitions state_start inputsym s state_end (remplace_add (Empilage(stacksym)) z)) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                        | Change(e) -> override_outcome (build_default_transitions state_start inputsym s e (Empilage(stacksym))) e @ (disassemble bl q state_start inputsym stacksym state_end replac))
      | (Next(_), (s, b) :: q) -> (match b with
                | Next(blist) | Top(blist) | State(blist) -> (disassemble b blist state_start s stacksym state_end replac) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                | Instruction(i) -> match i with 
                        | Pop -> (build_default_transitions state_start s stacksym state_end Empty) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                        | Reject -> failwith "The word is rejected!"
                        | Push(z) -> (build_default_transitions state_start s stacksym state_end (remplace_add (Empilage(stacksym)) z)) @ (disassemble bl q state_start inputsym stacksym state_end replac)
                        | Change(e) -> override_outcome (build_default_transitions state_start s stacksym e (Empilage(stacksym))) e @ (disassemble bl q state_start inputsym stacksym state_end replac))
      | (_,_) -> failwith "Non géré"
  in match bl with
    | State(blist) | Top(blist) | Next(blist) -> disassemble bl blist (Symbol(Epsilon)) (Symbol(Epsilon)) (Symbol(Epsilon)) (Symbol(Epsilon)) (Empilage(get_init_stack decl))
    | Instruction(_) -> failwith "Non géré (CODE CONV.BLDTRANS.FIRSTLEVEL.INSTR)"
in match auto with
  | Automate2(decl, bl) -> let trlist =  build_transitions decl bl in Automate(decl, trlist)


