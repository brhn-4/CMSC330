open Funs

(***
Brandon Nguyen
116621335
10/8/2020
Van Horn
***)

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target =
  fold(fun a x -> if x = target then a+1 else a) 0 lst
  ;;

let uniq lst = 
  fold (fun a x -> if ((count_occ a x) != 0) then a else x :: a) [] lst
  ;;


let assoc_list lst = 
  let ulist = uniq lst in 
  fold (fun a x -> ((x,count_occ lst x):: a)) [] ulist
  ;;

let ap fns args =
  fold(fun a b -> a@(map b args)) [] fns
  ;;
