(* #1 *)

let rec pow x n = 
    if n=0 then 1
    else x * pow x (n-1);;

pow 5 4;;

let rec float_pow x n = 
    if n = 0 then 1.0
    else x *. float_pow x (n-1);;

(* #2 *)

let rec compress list = 
    match list with 
    |[] -> []
    |[hd] -> [hd]
    |hd1 :: hd2 :: tl -> 
            let n_tl = compress(hd2 :: tl) in 
            if hd1 = hd2 then n_tl else hd1 :: n_tl;;

(* #3 *)

let rec remove_if list f =
    match list with 
    |[] -> []
    |hd :: tl -> 
            let is_true = f hd in 
            if is_true then remove_if tl f else hd :: (remove_if tl f)
    ;;


(* #4 *)
let rec slice list i j = 
    match list with 
    |[] -> []
    |hd :: tl -> 
            let tail = if j = 1 then [] else slice tl (i - 1) (j - 1) in 
            if i > 0 then tail else hd :: tail
;;

(* helper function for #5 *)
let rec reverse_list list = 
    let rec helper new_list = function 
        |[] -> new_list
        |hd :: tl -> 
                helper (hd :: new_list) tl in   
    helper [] list;;


(* #5 NEED HELP*)
let equivs f list = 
    let rec helper current acc  = function 
        |[] -> []
        |[x] -> (x :: current) :: acc
        |a :: (b :: _ as t) -> 
                let is_true = f a b in
                if is_true then helper ( a :: current) acc t 
                else helper [] ((a :: current) :: acc) t in 
    reverse_list(helper [] [] list)
;;


(* #6 *)

let is_prime n = 
    let rec isZero x d = 
        match d with
        |1 -> true
        |_ -> (x mod d <> 0) && isZero x (d-1) in 
    match n with
    |0 | 1 -> false
    |_ -> isZero n (n-1);;



let goldbachpair n = 
    let rec helper d = 
        if is_prime d && is_prime (n-d) then (d, n-d)
        else helper (d+1) in 
    helper 2;;
        
(* #7 *)
let rec equiv_on f g lst = 
    match lst with
    |[] -> true 
    |hd :: tl -> 
            let is_true = (f hd) = (g hd) in
            if is_true then equiv_on f g tl else false
;;

(* #8  check with different compare functions*)
let rec pairwisefilter cmp lst = 
    match lst with
    |[] -> []
    |[a] -> [a]
    |a :: b:: tl -> 
            let result = cmp a b in
            result :: (pairwisefilter cmp tl);;

(* #9 inomplete:: need help*)
let rec exponent base exp = 
    match exp with 
    |0 -> 1
    |1 -> base * (exponent base (exp - 1)) 
    |n -> 
            if n < 0 then 0 
            else base * (exponent base (exp -1 ));;

(*
let rec polynomial lst = 
    match lst with 
        |[] -> (fun x -> x * 0) (* function that results in 0*) 
        |hd :: tl -> 
            let coef = fst hd in 
            let exp = snd hd in
            (fun x -> coef * (exponent x exp))input + polynomial tl ;; 
*)

(* #10 ask if this is allowed *)
let rec powerset = function 
    |[] -> [[]]
    |hd :: tl -> 
            let ps = powerset tl in 
            ps @ List.map (fun ss -> hd :: ss) ps;;

