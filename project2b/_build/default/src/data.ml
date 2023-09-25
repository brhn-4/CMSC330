open Funs

(***
Brandon Nguyen
116621335
10/8/2020
Van Horn
***)

(************************)
(* Part 2: Integer BSTs *)
(************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = 
  match t with 
  | IntLeaf -> 0
  | IntNode(_,left,right) -> (int_size left) + (int_size right) + 1
  ;;

let rec int_max t = 
  if int_size t = 0 then invalid_arg "int_max"
  else
    match t with
    | IntLeaf -> invalid_arg "int_max"
    | IntNode (v,left,right) when right = IntLeaf -> v
    | IntNode (v,left,right) -> int_max right
    ;;

(****************************)
(* Part 3: Polymorphic BSTs *)
(****************************)

type 'a atree =
  | Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec pinsert x t = 
  let (comp, tree) = t in
  match tree with
  | Leaf -> (comp, Node(x, Leaf, Leaf))
  | Node (v,left,right) when (comp x v) < 0 -> let (comp2,left_sub) = pinsert x (comp,left) in
    (comp2, Node(v,left_sub,right))
  | Node (v,left,right) when (comp x v) > 0 -> let (comp2,right_sub) = pinsert x (comp,right) in
    (comp2, Node(v,left,right_sub))
  | Node (v,left,right) when (comp x v) = 0 -> (comp,Node(v,left,right))
  | _ -> invalid_arg "catch all"
  ;;


let rec pmem x t = 
  let (comp, tree) = t in
  match tree with
  | Leaf -> false
  | Node (v,left,right) when (comp x v) < 0 -> pmem x (comp,left)
  | Node (v,left,right) when (comp x v) > 0 -> pmem x (comp,right)
  | Node (v,left,right) -> true
  ;;

let pinsert_all lst t = 
  fold_right (fun x acc -> pinsert x acc) (rev lst) t
  ;;

let rec p_as_list t = 
  let (comp, tree) = t in
  match tree with 
  | Leaf -> []
  | Node (v,left,right) ->
    p_as_list (comp,left) @ [v] @ p_as_list (comp,right)
    ;;
    (*left, middle right traversal*)

let pmap f t = 
  let lst = p_as_list t in
  let (comp, tree) = t in
  let lst_new = map f lst in
  pinsert_all lst_new (comp, Leaf);;
  

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (((string * int) list) list);;

let empty_table () : lookup_table = [];;

let push_scope (table: lookup_table) : lookup_table = 
  match table with
  | [] -> [[]]
  | h::t -> []:: (h::t) 
  ;;

let pop_scope (table: lookup_table) : lookup_table = 
  match table with
  | [] -> failwith "No scopes remain!"
  | h::t -> t
  ;;

let add_var name value (table: lookup_table) : lookup_table = 
  match table with
  | [] -> failwith "There are no scopes to add a variable to!"
  | h::t -> let recent_scope = h in
    ((name,value) :: recent_scope)::t
    ;;

 let rec lookup_helper tup_lst name = 
  match tup_lst with
   | [] -> None
   | h::t -> let (n,v) = h in if n = name then(Some v) else lookup_helper t name
   ;;
    
let rec lookup name (table: lookup_table) = 
  match table with
  | [] -> failwith "Variable not found!"
  | h::t -> (match lookup_helper h name with
            | None -> (lookup name t)
            | Some v -> v )
  ;;




(*******************************)
(* Part 5: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int };;
type shape =
  | Circ of { radius: float; center: pt }
  | Square of { length: float; upper: pt }
  | Rect of { width: float; height: float; upper: pt }
;;

(* Implement the functions below. *)

let area s = 
  match s with
  | Circ temp-> 3.14 *. temp.radius *. temp.radius
  | Square temp -> temp.length *. temp.length
  | Rect temp -> temp.width *. temp.height
  ;;

let filter f lst = 
  fold (fun a x -> if (f x) then x::a else a ) [] lst
  ;;
