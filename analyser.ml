open Ast
open Printf

let analyse_init_stack (auto:automate) = 
  let decl = get_declaration auto in
  let init_stack = get_init_stack decl in
  let accepted_stack = get_stack_symbols decl in
  if not (List.mem init_stack accepted_stack)
    then let error = sprintf("Unaccepted initial stack: %s it should be in {%s}.")
    (as_string_sym init_stack) (as_string_sym_list accepted_stack) in failwith error
    else printf("Initial stack is okay.\n")

let analyse_init_state (auto:automate) =
  let decl = get_declaration auto in
  let init_state = get_init_state decl in
  let accepted_state = get_state_symbols decl in
  if not (List.mem init_state accepted_state)
    then let error = sprintf("Unaccepted initial state: %s it should be in {%s}.")
    (as_string_sym init_state) (as_string_sym_list accepted_state) in failwith error
    else printf("Initial state is okay.\n")

let analyse_transistions_information (auto:automate) =

  let rec remplacement_as_list (rempl:remplacement) (acc:stacksymbols) :stacksymbols = match rempl with
  | Empty -> acc
  | Empilage x -> x::acc
  | Remplace (x, xl) -> remplacement_as_list xl (x::acc) 
  in 
  let decl = get_declaration auto in
  let transis = get_transitions auto in
  let accepted_input = (get_input_symbols decl) @ [Symbol(Epsilon)] in
  let accepted_stack = get_stack_symbols decl in 
  let accepted_state = get_state_symbols decl in 
  let _ = List.map (fun transi -> if 
    not (List.mem (get_necessary_input transi) accepted_input)
    || not (List.mem (get_new_state transi) accepted_state)
    || not (List.mem (get_necessary_state transi) accepted_state)
    || not (List.mem (get_necessary_top transi) accepted_stack)
    || let rempl_sym_list = remplacement_as_list (get_remplacement transi) [] in 
      (List.exists (fun remplace_symbol -> match remplace_symbol with
      | Symbol(x) -> match x with 
        | Epsilon -> false 
        | _ -> not (List.mem remplace_symbol accepted_stack)) rempl_sym_list)
    then let error = sprintf("A symbol in this transition: %s is not declared.") (as_string_transi transi) in failwith error
    else ()) transis
  in printf "Transitions seem good.\n"

let analyse_determinization (auto:automate) =
  let transis = get_transitions auto in
  let _ = List.map
    (fun t1 -> List.map (fun t2 ->
      if get_necessary_state t1 = get_necessary_state t2 
      && get_necessary_input t1 = get_necessary_input t2
      && get_necessary_top t1 = get_necessary_top t2 
      && (get_new_state t1 <> get_new_state t2 || get_remplacement t1 <> get_remplacement t2)
      then let error = sprintf("Indetermination with %s and %s") (as_string_transi t1) (as_string_transi t2) in
      failwith error
      else ()) transis)
    transis  in printf("Determinization is okay.\n")