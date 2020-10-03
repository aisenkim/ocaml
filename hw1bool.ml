(* Question 2.1 *)
type bool_expr = 
    |Lit of string
    |Not of bool_expr
    |And of bool_expr * bool_expr
    |Or of bool_expr * bool_expr
;;

let rec evaluate a avalue b bvalue = function
    |Lit x -> 
            if x = a then avalue
            else bvalue
    |Not e -> 
            not(evaluate a avalue b bvalue e)
    |And(e1, e2) -> 
            evaluate a avalue b bvalue e1 && evaluate a avalue b bvalue e2 
    |Or(e1, e2) -> 
            evaluate a avalue b bvalue e1 || evaluate a avalue b bvalue e2 ;;

let truth_table a b expr = 
    [(true, true, evaluate a true b true expr);
    (true, false, evaluate a true b false expr);
    (false, true, evaluate a false b true expr);
    (false, false, evaluate a false b false expr)];;
    
