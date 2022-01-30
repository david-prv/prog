let a = Array.make 3 1 ;;
let b = a.(1) <- 2 ;;

(* Sequences *)
let newArr = Array.make 3 1 in newArr.(1) <- 2; newArr.(2) <- 3; newArr ;;

let rec swap a i j= 
  let x= a.(i) in
  a.(i)<-a.(j);a.(j)<-x                                 

let ssort1 a : unit =
  let r = Array.length a - 1 in
  let rec min k j : int =
    if k >= r then j
    else if a.(k+1) < a.(j) then min (k+1) (k+1)
    else min (k+1) j
  in let rec loop i : unit =
       if i >= r then ()
       else (swap a (min i i) i; loop (i+1))
  in loop 0
