(*** IIT CS 440, Spring 2021 ***)

(** DFA Simulator *)

(* Ignore these next two lines... *)
module D : ParseDFA.DFA_Types =
  struct

    (* These are the type definitions for DFAs *)
    type symbol = char
    type state = int
    type dfa = { states : int; (* Number of states *)
                 (* States are numbered 0 through states
                  * State 0 is the start state *)
                 accept : state list; (* List of accept states *)
                 alpha  : symbol list; (* Alphabet of the DFA *)
                 trans  : (state * symbol * state) list
                   (* List of transitions in the form
                    * (from state, transition symbol, to state) *)
               }

                 (* Ignore the next few lines too. *)
  end

open D
module Parser = ParseDFA.Parser(D)

(* OK, stop ignoring. *)

exception ImplementMe
exception IllformedInput of string

(*>* Problem 2.1 *>*)

(* Returns the new state after seeing symbol symb in state "state".
 * trans_list is the list of transitions for the DFA
 * Raises IllformedInput with an informative message if the state and
 * symbol aren't found in the transition list *)
let rec transition (trans_list : (state * symbol * state) list)
          (symb : symbol) (state: state) : state =
              match trans_list with
              | [] -> raise (IllformedInput "state and symbol pair isn't found")
              | h::t ->
                      let (st1, sy, st2) = h in
                      if (st1 = state && sy = symb) then st2
                      else transition t symb state
;;

let test_dfa = {
    states = 4;
    accept = [2; 3];
    alpha = ['a'; 'b'];
    trans = [(0, 'b', 0); (0, 'a', 1);
            (1, 'a', 1); (1, 'b', 2);
            (2, 'b', 2); (2, 'a', 3);
            (3, 'a', 3); (3, 'b', 2)]
};;

assert(transition test_dfa.trans 'a' 0 = 1);;
assert(transition test_dfa.trans 'b' 1 = 2);;
assert(transition test_dfa.trans 'a' 1 = 1);;
assert(transition test_dfa.trans 'a' 2 = 3);;
assert(try let _ = transition test_dfa.trans 'c' 2 in false with IllformedInput _ -> true);;

(*>* Problem 2.2 *>*)

(* Return true if dfa accepts input starting from state, false otherwise
 * Raises IllformedInput with an informative message if a symbol in the
 * input isn't in the alphabet. *)
let rec dfa_sim (dfa: dfa) (state: state) (input: symbol list) : bool =
    match input with
    | [] ->
            if List.mem state dfa.accept then true
            else false
    | h::t ->
            if not (List.mem h dfa.alpha) then raise (IllformedInput "input symbol isn't in the alphabet")
            else dfa_sim dfa (transition dfa.trans h state) t
;;

assert(dfa_sim test_dfa 0 ['b'; 'a'; 'a'; 'b'] = true);;
assert(dfa_sim test_dfa 0 ['b'; 'a'; 'a'; 'b'; 'a'] = true);;
assert(dfa_sim test_dfa 0 ['a'; 'a'] = false);;
assert(try let _ = dfa_sim test_dfa 0 ['a'; 'a'; 'c'] in false with IllformedInput _ -> true);;
