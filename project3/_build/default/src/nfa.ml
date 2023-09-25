open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []
;;

let rec fold f a xs =
 match xs with
  | [] -> a
  | x :: xt -> fold f (f a x) xt
;;

let parse_deltas (nfa: ('q,'s) nfa_t) (state: 'q) (link: 's option) =
  List.sort_uniq compare (fold(fun acc x -> let (start,connect,fin) = x in 
      if (start = state) && (link = connect) then
         x::acc  
      else 
        acc
      ) [] nfa.delta )
;;

let parse_deltas_ (delta: ('q, 's) transition list) (state: 'q) (link: 's option) =
  List.sort_uniq compare (fold(fun acc x -> let (start,connect,fin) = x in 
      if (start = state) && (link = connect) then
         x::acc  
      else 
        acc
      ) [] delta )
;;


(****************)
(* Part 1: NFAs *)
(****************)


let rec move_helper (delta: ('q, 's) transition list)  (qs: 'q list) (s: 's option) =
  fold (fun acc x -> (fold (fun acc2 (src, link, dest) -> if src = x && link = s then dest::acc2 else acc2) [] delta) @ acc) [] qs
;;
                



let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  match s with 
  | None -> List.sort_uniq compare (move_helper nfa.delta qs s) 
  | Some x -> if List.mem x nfa.sigma then List.sort_uniq compare (move_helper nfa.delta qs s) else []
  (* if (List.mem x nfa.sigma)  then [] else move_helper nfa.delta qs s []*)
;;



let rec closure_helper (delta: ('q, 's) transition list) (state: 'q) = 
  match parse_deltas_ delta state None with
  | [] -> [state]
  | _ -> fold (fun acc (src, link, dest) -> List.sort_uniq compare ((closure_helper delta dest) @ acc)) [state] (parse_deltas_ delta state None)
;;
 


;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  List.sort_uniq compare (fold (fun acc x -> (List.sort_uniq compare (closure_helper nfa.delta x)) @ acc ) [] qs)
;;



(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  fold(fun acc x ->  let next_states = List.sort_uniq compare (move nfa qs (Some x)) in 
                     let next_e_close = List.sort_uniq compare ((e_closure nfa next_states) @ next_states)in
                     (List.sort_uniq compare next_e_close)::acc) [] nfa.sigma
;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold (fun acc x ->  let next_states = List.sort_uniq compare (move nfa qs (Some x)) in
                      let next_e_close = (e_closure nfa next_states) @ next_states in
                      (qs, Some x, (List.sort_uniq compare next_e_close))::acc) [] nfa.sigma
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if fold (fun acc x -> if List.mem x nfa.fs || acc then true else false) false qs then [qs] else []
;;


let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list): ('q list, 's) nfa_t =
    
  match work with
  | [] -> { sigma = nfa.sigma;
             qs = dfa.qs;
             q0 = (e_closure nfa [nfa.q0]);
             fs = fold (fun acc x -> (new_finals nfa x) @ acc) [] dfa.qs;
             delta = dfa.delta;
          }
  | h::t -> let trans = List.sort_uniq compare (new_trans nfa h) in
            let states = List.sort_uniq compare (new_states nfa h) in
            let new_qs = List.sort_uniq compare states@dfa.qs in
            let new_dfa = {
              sigma = [];
              qs = new_qs;
              q0 = e_closure nfa [nfa.q0];
              fs = [];
              delta = List.sort_uniq compare (trans@dfa.delta);
              } in
              nfa_to_dfa_step nfa new_dfa (List.sort_uniq compare (fold(fun acc x -> if (List.mem x t || List.mem x dfa.qs) then acc else x::acc) t states))
;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let start = e_closure nfa [nfa.q0] in
  let dfa = {
    sigma = nfa.sigma;
    qs = [start];
    q0 = start;
    fs = [nfa.fs];
    delta = []
  } in 
  nfa_to_dfa_step nfa dfa [start]
;;
  

(*accept*)

let rec accept_helper (dfa: ('q,char) nfa_t) (s:char list) (curr_state: 'q) =
  match s with
  | [] -> if List.mem curr_state dfa.fs then true else false;
  | h::t -> let delta_list = parse_deltas dfa curr_state (Some h) in
            match delta_list with
            | [] -> false
            | h2::t2 -> let (start, link, fin) = h2 in accept_helper dfa t fin

;;
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let dfa = nfa_to_dfa nfa in
  let new_s = explode s in
  let curr_state = dfa.q0 in

  accept_helper dfa new_s curr_state
;;