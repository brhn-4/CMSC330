(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = let (a,b,c) = tup in (c,b,a);;

let abs x = if x >= 0 then x else (-x);;

let area x y = 
  let (x1, y1) = x in
  let (x2, y2) = y in
  abs(x2 - x1) * abs(y2 - y1)
  ;;

let volume x y = 
  let(x1,y1,z1) = x in
  let(x2,y2,z2) = y in
  abs(x1-x2)*abs(y1-y2)*abs(z1-z2)
  ;;


(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = 
  if (x = 0 || x = 1) then 1 else x * factorial (x-1);;

let rec pow x y = 
  if y = 0 then 1 else x * pow x (y-1);;


let rec log x y = 
  if x > y then
   0
  else
    1 + log x (y/x);;

let rec is_prime_helper x y =
  if y = 1 then
    true
  else if x mod (y) = 0 then
    false
  else
    is_prime_helper x (y-1);;

let rec is_prime x = 
  if x > 1 then
    is_prime_helper x (x-1) 
  else
    false;;

let rec next_prime_helper x = 
  if is_prime x then
    x
  else  
    next_prime_helper (x+1)
  ;;


let rec next_prime x = 
  if is_prime x then 
    x
  else
    next_prime_helper x
  ;;




(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_helper idx lst curr = 
  if (curr > idx) then
    failwith "Out of bounds"
  else
    match lst with
    | [] -> failwith "Out of bounds"
    | h::t -> if (curr = idx) then h else get_helper idx t (curr + 1);;

let rec get idx lst = get_helper idx lst 0;;

let rec larger_helper lst =
  match lst with
  | [] -> 0
  | h::t -> 1 + larger_helper t 

let larger lst1 lst2 = 
  if larger_helper lst1 > larger_helper lst2 then
    lst1
  else if larger_helper lst2 > larger_helper lst1 then
    lst2
  else
    [];;

let rec reverse_helper lst rev =
  match lst with
  | [] -> rev
  | h::t -> reverse_helper t (h::rev);;

let rec reverse lst = reverse_helper lst [];;

let rec combine lst1 lst2 = 
  match lst1, lst2 with
  | [],[] -> []
  | [],_ -> lst2
  | h::t,_ -> h :: (combine t lst2)

let rec rotate_helper shift lst idx =
  let length = (larger_helper lst) in
  if (idx >= length) then
   []
  else
  (get ((idx + shift) mod length) (lst)) :: (rotate_helper (shift) (lst) (idx));;

let rec rotate shift lst = rotate_helper shift lst 0;;
