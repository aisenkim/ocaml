(* 2.2 *)

type expr = 
    |Const of int 
    |Var of string 
    |Plus of node    
    |Mult of node
    |Minus of node 
    |Div of node 
and node = {
    arg1 : expr;
    arg2 : expr
    };;

let rec evaluate = function
    |Const x -> x
    |Plus {arg1; arg2} -> 
            let num1 = evaluate arg1 in 
            let num2 = evaluate arg2 in 
            num1 + num2
    |Minus {arg1; arg2} -> 
            let num1 = evaluate arg1 in 
            let num2 = evaluate arg2 in 
            num1 - num2
    |Mult {arg1; arg2} -> 
            let num1 = evaluate arg1 in 
            let num2 = evaluate arg2 in
            num1 * num2
    |Div {arg1; arg2} -> 
            let num1 = evaluate arg1 in 
            let num2 = evaluate arg2 in 
            num1 / num2;;
