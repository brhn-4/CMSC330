open TokenTypes


let tokenize input =
let rec helper pos str_ =
  if pos >= (String.length str_) then
    [EOF]
  else
    if (Str.string_match  (Str.regexp "\\+") str_ pos) = true then
        Tok_Add :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "[-]?[0-9]+") str_ pos) = true then
        let int_matched = String.trim (Str.matched_string str_) in
        let length = String.length int_matched in
        (Tok_Int (int_of_string int_matched)) :: (helper (pos + length) str_)
    else if (Str.string_match  (Str.regexp "-") str_ pos) = true then
        Tok_Sub :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "\\*") str_ pos) = true then
        Tok_Mult :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "/") str_ pos) = true then
        Tok_Div :: (helper (pos + 1) str_)
    else if (Str.string_match   (Str.regexp "==") str_ pos) = true then
        Tok_Equal :: (helper (pos + 2) str_)
    else if (Str.string_match  (Str.regexp "!=") str_ pos) = true then
        Tok_NotEqual :: (helper (pos + 2) str_)
    else if (Str.string_match  (Str.regexp "\\^") str_ pos) = true then
        Tok_Pow :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "(") str_ pos) = true then
        Tok_LParen :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp ")") str_ pos) = true then
        Tok_RParen :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp ">=") str_ pos) = true then
        Tok_GreaterEqual :: (helper (pos + 2) str_)
    else if (Str.string_match  (Str.regexp "<=") str_ pos) = true then
        Tok_LessEqual :: (helper (pos + 2) str_)
    else if (Str.string_match  (Str.regexp ">") str_ pos) = true then
        Tok_Greater :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "<") str_ pos) = true then
        Tok_Less :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "||") str_ pos) = true then
        Tok_Or :: (helper (pos + 2) str_)
    else if (Str.string_match  (Str.regexp "&&") str_ pos) = true then
        Tok_And :: (helper (pos + 2) str_)
    else if (Str.string_match  (Str.regexp "!") str_ pos) = true then
        Tok_Not :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "{") str_ pos) = true then
        Tok_LBrace :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "}") str_ pos) = true then
        Tok_RBrace :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp ";") str_ pos) = true then
        Tok_Semi :: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str_ pos) = true then
        let matched_str = String.trim(Str.matched_string str_) in
        let length = String.length matched_str in
        if (matched_str = "main") then
          Tok_Main :: (helper (pos + 4) str_)
        else if (matched_str = "printf") then
          Tok_Print :: (helper (pos + 6) str_)
        else if (matched_str = "bool") then
          Tok_Bool_Type :: (helper (pos + 4) str_)
        else if (matched_str = "int") then
          Tok_Int_Type :: (helper (pos + 3) str_)
        else if (matched_str = "to") then
          Tok_To :: (helper (pos + 2) str_)
        else if (matched_str = "from") then 
          Tok_From :: (helper (pos + 4) str_)
        else if (matched_str = "for") then
          Tok_For :: (helper (pos + 3) str_)
        else if (matched_str = "else") then 
          Tok_Else :: (helper (pos + 4) str_)
        else if (matched_str = "while") then 
          Tok_While :: (helper (pos + 5) str_)
        else if (matched_str = "if") then
          Tok_If :: (helper (pos + 2) str_)
        else if (matched_str =  "true") then
         (Tok_Bool (true)) :: (helper (pos + 4) str_)
        else if (matched_str = "false") then  
          (Tok_Bool (false)) :: (helper (pos + 5) str_)
        else
          (Tok_ID (matched_str)) :: (helper (pos +length) str_)
    else if (Str.string_match  (Str.regexp "=") str_ pos) then
        Tok_Assign:: (helper (pos + 1) str_)
    else if (Str.string_match  (Str.regexp "[ \t\n]") str_ pos) then
        (helper (pos + 1) str_)
    else
      raise (InvalidInputException "invalid input")
    in helper 0 input;;
