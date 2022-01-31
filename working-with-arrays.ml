let rec swap a i j= 
  let x= a.(i) in
  a.(i)<-a.(j);a.(j)<-x                                 
;;

let ssort a : unit =
  let r = Array.length a - 1 in
  let rec min k j : int =
    if k >= r then j
    else if a.(k+1) < a.(j) then min (k+1) (k+1)
    else min (k+1) j
  in let rec loop i : unit =
       if i >= r then ()
       else (swap a (min i i) i; loop (i+1))
  in loop 0
;; 

let fst (x,y) = x ;;

let rec iter f n x =
  if n < 1 then x
  else iter f (n - 1) (f x)
;;

let fibi n = fst (iter (fun (a,b) -> (b, a + b)) n (0,1)) ;; 

let reverse a : unit =
  let rec loop i j =
    if i > j then ()
    else (swap a i j; loop (i+1) (j-1))
  in loop 0 (Array.length a - 1)
;;
    
let sorted a : bool =
  let r = Array.length a - 1 in
  let rec loop i : bool =
    if r <= i then true
    else if a.(i) <= a.(i+1) then loop (i+2) else false 
  in loop 0
;;

let init (n:int) f =
  let a = Array.make n (f 0) in
  let rec loop i =
    if i >= (n-1) then (a.(n-1) <- f (n-1);a)
    else (a.(i) <- f i; loop (i+1)) 
  in loop 1
;;

let fib' (n:int) =
  init (n+1) (fun k -> fibi k) ;;

let clamp a : unit =
  let r = Array.length a in
  let rec loop i : unit =
    if i >= r then ()
    else (if a.(i) > 100 then a.(i) <- 100 else if a.(i) < 0 then a.(i) <- 0 else (); loop (i+1))
  in loop 0
;;

(*
  invalid_arg "1" + invalid_arg "2";;
  (invalid_arg "1", invalid_arg "2");;
  (invalid_arg "1"; invalid_arg "2");;
  (failwith "1" * failwith "2") + failwith "3";;
  failwith "1" * (failwith "2" + failwith "3");;
*)
