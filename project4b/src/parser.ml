open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))



(* Parsing (TODO: implement your code below) *)

(* FOLLOW CFG BELOW
Expr -> OrExpr
OrExpr -> AndExpr || OrExpr | AndExpr
AndExpr -> EqualityExpr && AndExpr | EqualityExpr
EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
  EqualityOperator -> == | !=
RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
  RelationalOperator -> < | > | <= | >=
AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
  AdditiveOperator -> + | -
MultiplicativeExpr -> PowerExpr MultiplicativeOperator MultiplicativeExpr | PowerExpr
  MultiplicativeOperator -> * | /
PowerExpr -> UnaryExpr ^ PowerExpr | UnaryExpr
UnaryExpr -> ! UnaryExpr | PrimaryExpr
PrimaryExpr -> Tok_Int | Tok_Bool | Tok_ID | ( Expr )
*)

let rec parse_expr toks : expr_result =
  parse_or toks

  and parse_or toks = 
    let(toks_after_and, expr) = parse_and toks in
    match (lookahead toks_after_and) with
    | Tok_Or -> let toks2 =  match_token toks_after_and Tok_Or in  
                let (toks3,expr2) = parse_or toks2 in
                (toks3, Or(expr,expr2))
    | _ -> (toks_after_and, expr)

  and parse_and toks =
    let(toks_after_equality, expr) = parse_equality toks in
    match (lookahead toks_after_equality) with
    | Tok_And -> let toks2 = match_token toks_after_equality Tok_And in
                 let (toks3,expr2) = parse_and toks2 in
                 (toks3, And(expr,expr2))
    | _ -> (toks_after_equality, expr)
  
  and parse_equality toks =
    let(toks_after_relational, expr) = parse_relational toks in
    match (lookahead toks_after_relational) with
    | Tok_Equal -> let toks2 = match_token toks_after_relational Tok_Equal in
                   let (toks3,expr2) = parse_equality toks2 in  
                   (toks3, Equal(expr,expr2))
    | Tok_NotEqual -> let toks4 = match_token toks_after_relational Tok_NotEqual in 
                      let (toks5,expr3) = parse_equality toks4 in
                      (toks5, NotEqual(expr,expr3))
    | _ -> (toks_after_relational, expr)

  and parse_relational toks =
    let(toks_after_add, expr) = parse_add toks in
    match (lookahead toks_after_add) with
    | Tok_Greater -> let toks2 = match_token toks_after_add Tok_Greater in
                     let (toks3,expr2) = parse_relational toks2 in
                     (toks3,Greater(expr,expr2))
    | Tok_GreaterEqual -> let toks4 = match_token toks_after_add Tok_GreaterEqual in
                          let (toks5,expr3) = parse_relational toks4 in
                          (toks5,GreaterEqual(expr,expr3))
    | Tok_Less -> let toks6 = match_token toks_after_add Tok_Less in
                  let (toks7,expr4) = parse_relational toks6 in
                  (toks7,Less(expr,expr4))

    | Tok_LessEqual -> let toks8 = match_token toks_after_add Tok_LessEqual in
                       let (toks9,expr5) = parse_relational toks8 in
                       (toks9,LessEqual(expr,expr5))
    | _ -> (toks_after_add, expr)

  and parse_add toks = 
    let(toks_after_mult, expr) = parse_mult toks in
    match (lookahead toks_after_mult) with
    | Tok_Add -> let toks2 = match_token toks_after_mult Tok_Add in
                  let (toks3,expr2) = parse_add toks2 in
                  (toks3,Add(expr,expr2))
    | Tok_Sub ->  let toks4 = match_token toks_after_mult Tok_Sub in
                  let (toks5,expr3) = parse_add toks4 in
                  (toks5,Sub(expr,expr3))
    | _ -> (toks_after_mult, expr)

  and parse_mult toks =
    let(toks_after_pwr, expr) = parse_pwr toks in
    match (lookahead toks_after_pwr) with
    | Tok_Mult -> let toks2 = match_token toks_after_pwr Tok_Mult in
                  let (toks3,expr2) = parse_mult toks2 in
                  (toks3,Mult(expr,expr2))
    | Tok_Div -> let toks4 = match_token toks_after_pwr Tok_Div in
                 let (toks5,expr3) = parse_mult toks4 in
                 (toks5,Div(expr,expr3))
    | _ -> (toks_after_pwr, expr)
  
  and parse_pwr toks =
    let(toks_after_unary, expr) = parse_unary toks in
    match (lookahead toks_after_unary) with
    | Tok_Pow -> let toks2 = match_token toks_after_unary Tok_Pow in
                 let (toks3,expr2) = parse_pwr toks2 in
                 (toks3,Pow(expr,expr2))
    | _ -> (toks_after_unary, expr)

  and parse_unary toks = 
    match (lookahead toks) with
    | Tok_Not -> let toks2 = match_token toks Tok_Not in 
                 let (toks3,expr) = parse_unary toks2 in
                 (toks3,Not(expr))
    | _ -> parse_primary toks

  
  and parse_primary toks = 
    match (lookahead toks) with
    | Tok_Int i-> let toks2 = match_token toks (Tok_Int i) in (toks2,Int i)
    | Tok_Bool bool_-> let toks3 = match_token toks (Tok_Bool bool_) in (toks3,Bool bool_)
    | Tok_LParen -> let toks4 = (match_token toks Tok_LParen) in
                    let (toks5,expr) = parse_expr toks4 in
                    let toks6 = (match_token toks5 Tok_RParen) in
                    (toks6,expr)
    | Tok_ID id -> let toks7 = match_token toks (Tok_ID id) in (toks7, ID id)
    | _ -> raise (InvalidInputException "Error")

  
let bool_or_int toks =
  match lookahead toks with
  |Tok_Bool_Type -> "bool"
  |Tok_Int_Type -> "int"
  |_ -> raise(InvalidInputException "error")

let rec parse_stmt toks : stmt_result =
  match lookahead toks with
  | Tok_If -> let (toks2,expr) = parse_if toks in 
              let (toks3,expr2) = parse_stmt toks2 in
              (toks3,Seq(expr,expr2))
  | Tok_For -> let (toks2,expr) = parse_for toks in 
               let (toks3,expr2) = parse_stmt toks2 in
               (toks3,Seq(expr,expr2))
  | Tok_Int_Type -> let (toks2,expr) = parse_declare toks in 
                    let (toks3,expr2) = parse_stmt toks2 in
                    (toks3,Seq(expr,expr2))
  | Tok_Bool_Type -> let (toks2,expr) = parse_declare toks in 
                     let (toks3,expr2) = parse_stmt toks2 in
                     (toks3,Seq(expr,expr2))
  | Tok_Print ->  let (toks2,expr) = parse_print toks in 
                  let (toks3,expr2) = parse_stmt toks2 in
                  (toks3,Seq(expr,expr2))
  | Tok_ID id -> let (toks2,expr) = parse_assign toks in 
                 let (toks3,expr2) = parse_stmt toks2 in
                 (toks3,Seq(expr,expr2))
  | Tok_While -> let (toks2,expr) = parse_while toks in 
                 let (toks3,expr2) = parse_stmt toks2 in
                 (toks3,Seq(expr,expr2))
  | Tok_RBrace -> (toks,NoOp)
  | Tok_Semi -> (toks,NoOp)
  | EOF -> (toks,NoOp)
  | _ -> raise (InvalidInputException "parse error")

(*  FOLLOW CFG BELOW
Stmt -> StmtOptions Stmt | ε
  StmtOptions -> DeclareStmt | AssignStmt | PrintStmt | IfStmt | ForStmt | WhileStmt
DeclareStmt -> BasicType ID ;
BasicType -> int | bool
AssignStmt -> ID = Expr ;
PrintStmt -> printf ( Expr ) ;
IfStmt -> if ( Expr ) { Stmt } ElseBranch
ElseBranch -> else { Stmt } | ε
ForStmt -> for ( ID from Expr to Expr ) { Stmt }
WhileStmt -> while ( Expr ) { Stmt }
*)
  

  and parse_declare toks =
    if (bool_or_int toks) = "bool" then
      let toks2 = match_token toks (Tok_Bool_Type) in
      (match (lookahead toks2) with
      | Tok_ID id -> let toks3 = match_token toks2 (Tok_ID id) in
                     let toks4 = match_token toks3 (Tok_Semi) in
                     (toks4,Declare(Bool_Type, id))
      |_-> raise (InvalidInputException "error (parse_declare bool)"))
    else
      let toks5 = match_token toks (Tok_Int_Type) in
      (match (lookahead toks5) with
      | Tok_ID id -> let toks6 = match_token toks5 (Tok_ID id) in
                     let toks7 = match_token toks6 (Tok_Semi) in
                     (toks7,Declare(Int_Type,id))
      |_ -> raise (InvalidInputException "error (parse_declare int)"))
    


  and parse_assign toks = 
    let assign_id = lookahead toks in
    match assign_id with 
    | Tok_ID id -> let toks2 = match_token toks (assign_id) in
                  let toks3 = match_token toks2 (Tok_Assign) in
                  let (toks4, expr) = (parse_expr toks3) in
                  let toks5 = match_token toks4 Tok_Semi in
                  (toks5,Assign(id,expr))
   |_ -> raise (InvalidInputException "error")
  


and parse_print toks = 
  let toks2 = match_token toks Tok_Print in
  let toks3 = match_token toks2 Tok_LParen in

  let (toks4,expr) = (parse_expr toks3) in
  let toks5 = match_token toks4 Tok_RParen in
  let toks6 = match_token toks5 Tok_Semi in

  (toks6,Print(expr))


and parse_if toks = 
  let toks2 = match_token toks Tok_If in
  let toks3 = match_token toks2 Tok_LParen in
  let (toks4,expr) = (parse_expr toks3) in
  let toks5 = match_token toks4 Tok_RParen in
  let toks6 = match_token toks5 Tok_LBrace in
  let (toks7,stmt) = (parse_stmt toks6) in
  let toks8 = match_token toks7 Tok_RBrace in

  if ((lookahead toks8) = Tok_Else) then
    let toks_ = match_token toks8 Tok_Else in
    let toks9 = match_token toks_ Tok_LBrace in
    let (toks10,stmt2) = parse_stmt toks9 in
    let toks11 = match_token toks10 Tok_RBrace in
    (toks11,If(expr,stmt,stmt2))
  else
   (toks8,If(expr,stmt,NoOp))


and parse_for toks = 
  let toks2 = match_token toks Tok_For in
  let toks3 = match_token toks2 Tok_LParen in
  let assign_id = lookahead toks3 in
  match assign_id with
  |Tok_ID id -> let toks4 = match_token toks3 (assign_id) in
                let toks5 = match_token toks4  Tok_From in
                let (toks6,expr) = (parse_expr toks5) in
                let toks7 = match_token toks6 Tok_To in
                let (toks8,expr2) = (parse_expr toks7) in
                let toks9 = match_token toks8 Tok_RParen in
                let toks10 = match_token toks9 Tok_LBrace in
                let (toks11, stmt) = (parse_stmt toks10) in
                let toks12 = match_token toks11 Tok_RBrace in
                (toks12, For(id,expr,expr2,stmt))
  |_ -> raise (InvalidInputException "error")


and parse_while toks = 
  let toks2 = match_token toks Tok_While in
  let toks3 = match_token toks2 Tok_LParen in
  let (toks4,expr) = (parse_expr toks3) in
  let toks5 = match_token toks4 Tok_RParen in
  let toks6 = match_token toks5 Tok_LBrace in
  let (toks7,stmt) = (parse_stmt toks6) in
  let toks8 = match_token toks7 Tok_RBrace in 
  (toks8,While(expr,stmt))

(* FOLLOW CFG
Main -> int main ( ) { Stmt } EOF
*)
let parse_main toks : stmt =
  let toks2 = match_token toks Tok_Int_Type in
  let toks3 = match_token toks2 Tok_Main in
  let toks4 = match_token toks3 Tok_LParen in
  let toks5 = match_token toks4 Tok_RParen in
  let toks6 = match_token toks5 Tok_LBrace in 

  let (toks7,stmt) = (parse_stmt toks6) in
  let toks8 = match_token toks7 Tok_RBrace in
  if(toks8 <>[EOF]) then 
    raise (Failure "still toks")
  else
    stmt
