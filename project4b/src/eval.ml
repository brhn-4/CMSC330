open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec id_helper env id =
  match env with
  | [] -> raise (DeclareError "not a member")
  | (k,v) :: t -> if (k = id) then v else (id_helper t id)
;;
  


let rec eval_expr env t =
  match t with
  | Int x -> Int_Val(x) 
  | Bool b -> Bool_Val(b)
  | ID id -> (id_helper env id)
  | Add (exp1, exp2) ->  let e1 = (eval_expr env exp1) in
                        (match e1 with
                        | Bool_Val bool1 -> raise (TypeError "bool")
                        | Int_Val num1 -> let e2 = (eval_expr env exp2) in
                          (match e2 with
                          | Bool_Val bool2 -> raise (TypeError "bool")
                          | Int_Val num2 -> Int_Val(num1 + num2)))
  | Sub (exp1, exp2) ->  let e1 = (eval_expr env exp1) in
                          (match e1 with
                          | Bool_Val bool1 -> raise (TypeError "bool")
                          | Int_Val num1 -> let e2 = (eval_expr env exp2) in
                            (match e2 with
                            | Bool_Val bool2 -> raise (TypeError "bool")
                            | Int_Val num2 -> Int_Val(num1 - num2)))
  | Mult (exp1, exp2) ->  let e1 = (eval_expr env exp1) in
                            (match e1 with
                            | Bool_Val bool1 -> raise (TypeError "bool")
                            | Int_Val num1 -> let e2 = (eval_expr env exp2) in
                              (match e2 with
                              | Bool_Val bool2 -> raise (TypeError "bool")
                              | Int_Val num2 -> Int_Val(num1 * num2)))
 | Div (exp1, exp2) ->  let e1 = (eval_expr env exp1) in
                              (match e1 with
                              | Bool_Val bool1 -> raise (TypeError "bool")
                              | Int_Val num1 -> let e2 = (eval_expr env exp2) in
                                (match e2 with
                                | Bool_Val bool2 -> raise (TypeError "bool")
                                | Int_Val num2 -> if (num2 = 0) then raise DivByZeroError else Int_Val(num1/num2)))
  | Pow (expr,expr2) -> let e1 = (eval_expr env expr) in
                      (match e1 with
                      | Bool_Val bool1 -> raise (TypeError "bool")
                      | Int_Val num1 -> let e2 = (eval_expr env expr2) in
                        (match e2 with
                        | Bool_Val bool2 -> raise (TypeError "bool")
                        | Int_Val num2 -> Int_Val(int_of_float ((float_of_int num1) ** (float_of_int num2)))))
  | Or (expr,expr2) -> let e1 = (eval_expr env expr) in
                       let e2 = (eval_expr env expr2) in
                       (match e1 with 
                       | Bool_Val b1 -> ( match e2 with
                                          | Bool_Val b2 -> Bool_Val(b1 || b2)
                                          | _ -> raise (TypeError "not bool_val"))
                       | _-> raise (TypeError "not bool_val"))
  | And (expr,expr2) -> let e1 = (eval_expr env expr) in
                        let e2 = (eval_expr env expr2) in
                        (match e1 with 
                        | Bool_Val b1 -> ( match e2 with
                                          | Bool_Val b2 -> Bool_Val(b1 && b2)
                                          | _ -> raise (TypeError "not bool_val"))
                        | _-> raise (TypeError "not bool_val"))
  | Not expr -> let e1 = (eval_expr env expr) in
                (match e1 with
                | Bool_Val x -> if (x) then Bool_Val(false) else Bool_Val(true)
                | _ -> raise (TypeError "not bool_val"))
  | Greater (expr,expr2) ->  let e1 = (eval_expr env expr) in
                            (match e1 with
                            | Bool_Val bool2 -> raise (TypeError "not int")
                            | Int_Val num1 -> let e2 = (eval_expr env expr2) in
                              (match e2 with
                              | Bool_Val bool2 -> raise (TypeError "not int")
                              | Int_Val num2 -> if (num1 > num2) then Bool_Val(true) else Bool_Val(false)))
  | Less (expr,expr2) ->let e1 = (eval_expr env expr) in
                        (match e1 with
                        | Bool_Val bool2 -> raise (TypeError "not int")
                        | Int_Val num1 -> let e2 = (eval_expr env expr2) in
                          (match e2 with
                          | Bool_Val bool2 -> raise (TypeError "not int")
                          | Int_Val num2 -> if (num1 < num2) then Bool_Val(true) else Bool_Val(false)))
  | GreaterEqual (expr,expr2) -> let e1 = (eval_expr env expr) in
                                (match e1 with
                                | Bool_Val bool2 -> raise (TypeError "not int")
                                | Int_Val num1 -> let e2 = (eval_expr env expr2) in
                                  (match e2 with
                                  | Bool_Val bool2 -> raise (TypeError "not int")
                                  | Int_Val num2 -> if (num1 >= num2) then Bool_Val(true) else Bool_Val(false)))
  | LessEqual (expr,expr2) -> let e1 = (eval_expr env expr) in
                              (match e1 with
                              | Bool_Val bool2 -> raise (TypeError "not int")
                              | Int_Val num1 -> let e2 = (eval_expr env expr2) in
                                (match e2 with
                                | Bool_Val bool2 -> raise (TypeError "not int")
                                | Int_Val num2 -> if (num1 <= num2) then Bool_Val(true) else Bool_Val(false)))
  | Equal (expr,expr2) -> let e1 = (eval_expr env expr) in
                          let e2 = (eval_expr env expr2) in
                          (match e1 with
                          | Bool_Val bool1 -> (match e2 with
                            | Bool_Val bool2 -> if (bool1 = bool2) then Bool_Val(true) else Bool_Val(false)
                            | Int_Val num2 -> raise (TypeError "not bool"))
                          | Int_Val num1 -> (match e2 with
                            | Bool_Val bool3 -> raise (TypeError "not int")
                            | Int_Val num3 -> if (num1 = num3) then Bool_Val(true) else Bool_Val(false)))
                          
  | NotEqual (expr,expr2) ->let e1 = (eval_expr env expr) in
                            let e2 = (eval_expr env expr2) in
                            (match e1 with
                            | Bool_Val bool1 -> (match e2 with
                              | Bool_Val bool2 -> if (bool1 = bool2) then Bool_Val(false) else Bool_Val(true)
                              | Int_Val num2 -> raise (TypeError "not bool"))
                            | Int_Val num1 -> (match e2 with
                              | Bool_Val bool3 -> raise (TypeError "not int")
                              | Int_Val num3 -> if (num1 = num3) then Bool_Val(false) else Bool_Val(true)))



let rec eval_stmt env s =
  match s with
  | NoOp -> env
  | Seq (s1,s2) -> let env2 = (eval_stmt env s1) in
                   eval_stmt env2 s2
  | Declare (var,name) -> if(List.mem_assoc name env) then raise (DeclareError "member") else
                          (match var with
                          | Int_Type -> (name, Int_Val(0))::env
                          | Bool_Type -> (name, Bool_Val(false)):: env)
                      
  | Assign (name,expr) -> let e1 = eval_expr env expr in
                          let rec assign_helper name env =
                          (match env with
                          | [] -> raise (DeclareError "not a member")
                          | (k,v) :: t -> if (k = name) then v else (assign_helper name t))
                          in
                          (match (assign_helper name env) with
                          | Bool_Val bool1 -> (match e1 with
                                            | Bool_Val bool2 -> (name, Bool_Val (bool2)) :: (List.remove_assoc name env)
                                            | _ -> raise (TypeError "type mismatch"))
                          | Int_Val int1 -> (match e1 with
                                            | Int_Val int2 -> (name, Int_Val(int2)):: (List.remove_assoc name env)
                                            | _ -> raise (TypeError "type mismatch")))
  | If (expr, s1, s2) -> let e1 = eval_expr env expr in
                         (match e1 with
                         | Bool_Val bool1 -> if(bool1) then eval_stmt env s1 else eval_stmt env s2
                         | _ -> raise (TypeError "not bool"))
  | While (expr,s1) -> let e1 = eval_expr env expr in
                       (match e1 with
                       | Bool_Val bool1 ->if (bool1) then eval_stmt (eval_stmt env s1) (While(expr,s1)) else env
                       | _ -> raise (TypeError "not bool"))
  | For (id, expr1, expr2, s1) -> let new_env = (if (List.mem_assoc id env) then (match (List.assoc id env) with
                                                                            |Int_Val int1 -> env
                                                                            | _ ->raise (TypeError "not int"))
                                                else (eval_stmt env (Declare(Int_Type,id)))) in
                                  let e1 = eval_expr new_env expr1 in
                                  let e2 = eval_expr new_env expr2 in
                                  (match e1 with
                                   | Int_Val int1 -> (match e2 with
                                                      | Int_Val int2 -> if (int1 <= int2) then
                                                        let new_env_ = eval_stmt new_env (Assign(id,expr1)) in
                                                        let new_env2 = (eval_stmt new_env_ s1) in
                                                              (match List.assoc id new_env2  with
                                                              | Int_Val curr -> let new_env3 = eval_stmt new_env2 (Assign(id, Int(curr+1))) in
                                                                                               eval_stmt new_env3 (For(id, (Int(curr+1)), expr2,s1))
                                                              | _-> raise (TypeError "not int"))
                                                          else env
                                                                         
                                                      | _-> raise (TypeError "not int"))
                                  | _ -> raise (TypeError "not int")) 
  | Print (expr) -> let e1 = eval_expr env expr in
                    (match e1 with
                    | Int_Val int1 -> print_output_int(int1); print_output_newline(); env
                    | Bool_Val bool1 -> print_output_bool(bool1); print_output_newline();env
                   )
          
                                    

