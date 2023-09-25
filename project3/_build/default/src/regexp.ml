open List
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

let char_help x = 
  let src = fresh() in
  let dest = fresh() in
  {
    sigma = [x]; 
    qs = [src;dest]; 
    q0 = src; 
    fs =[dest]; 
    delta = [(src,Some x,dest)];
    }
;;

let union_help (nfa: (int,char) nfa_t) (nfa2: (int,char) nfa_t) =
  let src = fresh() in
  let dest = fresh() in
  let new_delta_1 = (src,None,(nfa.q0)) in
  let new_delta_2 = (List.hd nfa.fs,None,dest) in
  let new_delta_3 = (src, None, nfa2.q0) in
  let new_delta_4 = (List.hd nfa2.fs,None,dest) in
  {
    sigma = List.sort_uniq compare (nfa.sigma @ nfa2.sigma);
    qs = (List.sort_uniq compare (nfa.qs @nfa2.qs)) @ [src] @ [dest];
    q0 = src;
    fs = [dest];
    delta = (List.sort_uniq compare (nfa.delta @ nfa2.delta) 
    @ [new_delta_1] @ [new_delta_2] @ [new_delta_3] @ [new_delta_4]);
  }
  ;;

let cat_help (nfa: (int,char) nfa_t) (nfa2: (int,char) nfa_t) =
  let src = fresh() in
  let dest = fresh() in
  let new_delta_fin = (List.hd nfa2.fs,None,dest) in
  let new_delta_start = (src,None,nfa.q0) in
  let new_delta_mid = (List.hd nfa.fs, None, nfa2.q0) in
  {
    sigma = List.sort_uniq compare (nfa.sigma @ nfa2.sigma);
    qs = (List.sort_uniq compare (nfa.qs) @(nfa2.qs)) @ [src] @ [dest];
    q0 = src;
    fs = [dest];
    delta = new_delta_mid::(new_delta_start::(List.sort_uniq compare (nfa.delta)@(nfa2.delta))) @ [new_delta_fin];
  }
  
  ;;

let star_help (nfa: (int,char) nfa_t) =
  let src = fresh() in
  let dest = fresh() in
  let new_delta_1 = (src,None,nfa.q0) in
  let new_delta_2 = (List.hd nfa.fs ,None,dest) in
  let new_delta_3 = (src,None,dest) in
  let new_delta_4 = (dest,None,src) in
  {
    sigma = nfa.sigma;
    qs = List.sort_uniq compare (nfa.qs @ [src] @ [dest]);
    q0 = src;
    fs = [dest];
    delta = nfa.delta @ [new_delta_1] @ [new_delta_2] @ [new_delta_3] @ [new_delta_4];
  }

  ;;



let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  let start = fresh() in 
  match regexp with
  | Empty_String -> { sigma = []; qs = [start]; q0 = start; fs = [start];  delta = [];}
  | Char c -> char_help c
  | Union (a,b) -> union_help (regexp_to_nfa a) (regexp_to_nfa b)
  | Concat (a,b) -> cat_help (regexp_to_nfa a) (regexp_to_nfa b)
  | Star a -> star_help (regexp_to_nfa a)
;;

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
