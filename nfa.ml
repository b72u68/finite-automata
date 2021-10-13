(*** IIT CS 440, Spring 2021 ***)

(** NFA Simulator *)

(* Same deal here with ignoring some of these lines. *)
module N : ParseNFA.NFA_Types =
  struct

    type symbol = char
    type state = int
    (* Type is the same as before except now a transition has a
     * symbol option:
     * (m, Some s, n) is a transition from m to n on seeing symbol s
     * (m, None, n) is an epsilon-transition from m to n
     *)
    type nfa = { states : int;
                 accept : state list;
                 alpha  : symbol list;
                 trans  : (state * symbol option * state) list }

  end

open N
module Parser = ParseNFA.Parser(N)

exception IllformedInput of string
exception ImplementMe

(** Some useful helper functions **)

(* Sorts a list and removed duplicates; after you call norm on a list,
 * you can treat it like a set, that is, if (norm l1) = (norm l2), then l1
 * and l2 are equal as sets (have the same elements, regardless of order and
 * multiples)
 *)
let norm l =
  let rec dedup l =
    match l with
    | [] -> []
    | x::t -> x::(dedup (List.filter (fun x' -> x' <> x) t))
  in
  List.sort (fun a b -> a - b) (dedup l)
;;

(* Turns a list of states into a human-readable string. Useful for debugging *)
let string_of_states states =
  Printf.sprintf "{%s}"
    (String.concat ", " (List.map string_of_int states))
;;

(** Your code starts here **)

(*>* Problem 3.1 *>*)

(* Returns a list of states you can be in on seeing symbol symb (which is
 * either Some s for a symbol s, or None for epsilon) in state
 * "state". This is a list and not just a single state because this is
 * an NFA. Note that if symb is None, this should just be the states
 * reachable with one epsilon-transition. *)
let transitions (trans_list : (state * symbol option * state) list)
      (symb: symbol option) (state: state) : state list =
          let rec transition_rec trans_list symb state =
              match trans_list with
              | [] -> []
              | h::t ->
                      let (st1, sy, st2) = h in
                      if (st1 = state && sy = symb) then st2::(transition_rec t symb state)
                      else transition_rec t symb state
          in
          transition_rec trans_list symb state
;;

let test_nfa = {
    states = 5;
    accept = [2; 4];
    alpha = ['a'; 'b'];
    trans = [(0, None, 1); (0, None, 3);
            (1, Some 'a', 2);
            (3, Some 'b', 4);
            (2, Some 'a', 2);
            (4, Some 'b', 4)]
}
;;

assert(transitions test_nfa.trans (Some 'a') 4 = []);;
assert(transitions test_nfa.trans (Some 'b') 4 = [4]);;
assert(transitions test_nfa.trans None 0 = [1;3]);;
assert(transitions test_nfa.trans (Some 'a') 2 = [2]);;
assert(transitions test_nfa.trans (Some 'b') 1 = []);;
assert(transitions test_nfa.trans None 1 = []);;


(*>* Problem 3.2 *>*)

(* Returns the list of states accessible by (possibly multiple) epsilon
 * transitions from "states" *)
let rec eps_clos (nfa: nfa) (states: state list) : state list =
    let rec eps_dup nfa states passed_states acc =
        match states with
        | [] -> acc
        | h::t ->
                if List.mem h passed_states then eps_dup nfa t passed_states acc
                else eps_dup nfa (t @ (transitions nfa.trans None h)) (h::passed_states) ([h] @ (transitions nfa.trans None h) @ acc)
    in
    norm (eps_dup nfa states [] [])
;;

assert(eps_clos test_nfa [0] = [0;1;3]);;
assert(eps_clos test_nfa [2] = [2]);;
assert(eps_clos test_nfa [3;0] = [0;1;3]);;
assert(eps_clos test_nfa [1] = [1]);;
assert(eps_clos test_nfa [0;2] = [0;1;2;3]);;
assert(eps_clos test_nfa [0;1] = [0;1;3]);;

(*>* Problem 3.3 *>*)

let rec union l1 l2 =
    match l1 with
    | [] -> []
    | h::t ->
            if (List.mem h l2) then h::(union t l2)
            else union t l2
;;

let rec move nfa states' symb =
    match states' with
    | [] -> []
    | h::t -> (transitions nfa.trans (Some symb) h) @ (move nfa t symb)
;;

(* Returns true if nfa accepts input: "states" is the list of states we
 * might be in currently *)
let rec nfa_sim (nfa: nfa) (states: state list) (input: symbol list) : bool =
    let states' = eps_clos nfa states
    in
    match input with
    | [] ->
            if List.length (union states' nfa.accept) = 0 then false
            else true
    | h::t ->
            nfa_sim nfa (norm (eps_clos nfa (move nfa states' h))) t
;;
