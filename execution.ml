open Ast
open Printf

let execute (auto:automate) (input:inputsymbols) :unit = 
  let rec pop (stack:stacksymbols) (acc:stacksymbols) = match stack, acc with
  | [], [] -> failwith("Empty stack, cannot pop")
  | x::[], acc -> List.rev acc
  | x::xs, acc -> pop xs (x::acc)
  | _ -> failwith("J'espere qu'on arrive pas la")

  and get_declaration (auto:automate) :declaration = match auto with
  | Automate(decl, transi) -> decl

  and get_init_stack (decl:declaration) :inputsymbol = match decl with
  | Declaration(_, _, _, _, s) -> s

  and get_init_state (decl:declaration) :state = match decl with
  | Declaration(_, _, _, state, _) -> state

  and get_transitions (auto:automate) :transition list = match auto with
  | Automate(decl, transis) -> transis

  and find_applicable_transition (stack:stacksymbols) (state:state) (input_c:inputsymbol) (transis: transition list) =
    let rec top_stack (stack:stacksymbols) :stacksymbol = match stack with
    | [] -> failwith("Empty stack")
    | x::[] -> x
    | x::xs -> top_stack xs

    and get_necessary_top (transi:transition) :stacksymbol = match transi with 
    | Transition(_, _, top, _, _) -> top

    and get_necessary_state (transi:transition) :state = match transi with 
    | Transition(state, _, _, _, _) -> state

    and get_necessary_input (transi:transition) :inputsymbol = match transi with 
    | Transition(_, input_c, _ , _, _) -> input_c

    in match transis with
    | [] -> failwith("No applicable transitions")
    | transi::follow ->
              if top_stack stack = get_necessary_top transi
              && state = get_necessary_state transi
              && input_c = get_necessary_input transi 
              then transi else find_applicable_transition stack state input_c follow

  and get_remplacement (transi:transition) :remplacement = match transi with
  | Transition(_, _, _, _, r) -> r

  and get_new_state (transi:transition) :state = match transi with
  | Transition(_, _, _, state, _) -> state

  and apply_transi (stack:stacksymbols) (state:state) (input:inputsymbols) (transi:transition) :(stacksymbols * state) =
    let rec remplace (stack:stacksymbols) (rempl:remplacement) :stacksymbols = match rempl with
    | Empty -> pop stack []
    | Empilage (s) -> (pop stack []) @ [s]
    | Remplace (s, sl) -> remplace (stack @ [s]) sl

    in remplace stack (get_remplacement transi), get_new_state transi

  and run (stack:stacksymbols) (state:state) (input:inputsymbols) (transis:transition list) :unit = match input, stack with 
  | [], [] -> printf "stack : %s\tstate : %s\tinput : %s\t\tValidated\n" 
              (as_string_sym_list stack) (as_string_sym state) (as_string_sym_list input)
  | [], stack -> let transi = find_applicable_transition stack state (Symbol Epsilon) transis
                in let tuple = apply_transi stack state [(Symbol Epsilon)] transi
                in let () = printf "stack : %s\tstate : %s\tinput : %s\ttransition prise : %s\n" 
                (as_string_sym_list stack) (as_string_sym state) (as_string_sym_list [(Symbol Epsilon)]) (as_string_transi transi)
                in run (fst tuple) (snd tuple) [] transis
  | input_c::follow, stack -> let transi = find_applicable_transition stack state input_c transis 
                      in let tuple = apply_transi stack state input transi
                      in let () = printf "stack : %s\tsttate : %s\tinput : %s\ttransition prise : %s\n" 
                      (as_string_sym_list stack) (as_string_sym state) (as_string_sym_list input) (as_string_transi transi)
                      in run (fst tuple) (snd tuple) follow transis

  in let transis = get_transitions auto
  in let stack = [get_init_stack (get_declaration auto)]
  in let state = get_init_state (get_declaration auto)

  in run stack state input transis