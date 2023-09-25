(*The following functions have been provided
  for your convenience*) 

  let rec map f xs = match xs with
  | [] -> []
  | x :: xt -> (f x)::(map f xt)
  
  let rec foldl f a xs = match xs with
  | [] -> a
  | x :: xt -> foldl f (f a x) xt
  
  let rec foldr f xs a = match xs with
  | [] -> a
  | x :: xt -> f x (foldr f xt a) 

  (* You may want to use these functions for stalin_sort_right *)
  let rec rev lst = match lst with 
    | [] -> [] 
    | x :: xt -> (rev xt) @ [x] 

  let rec get_last_element lst = 
    match lst with 
    | [x] -> x 
    | _ :: xt -> get_last_element xt 
    | [] -> failwith "empty list has no elements"
  

  (*This record type will be used for the 
  update_database problem*) 
  type student_information = 
    { 
        name : string;
        age : int; 
        gpa : float;
    } 

  (*Implement these functions*)
  let mul_thresh lst thresh = 
    foldl
    (fun (less, greater) x -> if x < thresh then (less*x,greater) else (less,greater*x))
    (1,1)
    lst;;

  let multi_map f lst = 
      map (fun x -> (map (fun y -> f y) x)) lst
  
  let update_database lst = 
      map(fun (n, a, g) -> {name = n; age = a; gpa = g;}) lst
  
  let stalin_sort lst = 
    match lst with
    | [] -> []
    | h :: t -> let (_, curr_lst) = foldl (fun (prev, curr_lst) x -> 
      if x > prev then     
        (x, curr_lst @ [x]) 
      else 
        (prev,curr_lst)) (h,[]) lst in curr_lst
      
  
  let stalin_sort_right lst = 
    match lst with
    | [] -> []
    | h :: t -> let(_,curr_lst) = foldr (fun x (prev, curr_lst) -> 
      if x > prev then 
        (x, curr_lst @ [x]) 
      else 
        (prev,curr_lst) ) lst (get_last_element lst, [])  in curr_lst
  