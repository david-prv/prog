type comparison = LE | EQ | GR

let comp x y : comparison =
  if x < y then LE
  else if x = y then EQ else GR                  
;; 

let rec swap a i j= 
  let x= a.(i) in
  a.(i)<-a.(j);a.(j)<-x                                 
;; 

let ssort a : unit =
  let r = Array.length a - 1 in
  let rec min k j : int =
    if k >= r then j (* j saves the current smallest element index, k is running var *)
    else if a.(k+1) < a.(j) then min (k+1) (k+1)
    else min (k+1) j
  in let rec loop i : unit =
       if i >= r then ()
       else (swap a (min i i) i; loop (i+1))
  in loop 0
;; 

(* iterating on k-1 *)
let ssortMax a : unit =
  let r = Array.length a - 1 in
  let rec max k j : int =
    if k < 1 then if a.(k) > a.(j) then k else j
    else if a.(k-1) > a.(j) then max (k-1) (k-1)
    else max (k-1) j
  in let rec loop i : unit =
       if i < 0 then ()
       else (swap a (max i i) i; loop (i-1))
  in loop r
;; 

(* analogue to ssort *)
let ssortMax' a=
  let r= Array.length a -1 in 
  let rec max k j= 
    if k<0 then j else
    if a.(k)>a.(j) then max (k-1) (k) else
      max(k-1) j 
  in
  let rec loop i =
    if i<0 then ()
    else (swap a (max i i) i ;loop (i-1)) in
  loop r
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
    else if a.(i) <= a.(i+1) then loop (i+1) else false 
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

let bsearch a x : int option =
  let arr = if sorted a then a else (ssort a; a) in
  let rec loop l r =
    if l > r then None
    else let m = (l+r)/2 in
      match comp x arr.(m) with
      | LE -> print_string "LE -> "; loop l (m-1)
      | EQ -> print_string "EQ"; Some m
      | GR -> print_string "GR -> "; loop (m+1) r
  in loop 0 (Array.length arr - 1)
;;

let bsearchF f x n : int option =
  let arr = init (n + 1) f in 
  let rec loop l r =
    if l > r then None
    else let m = (l+r)/2 in
      match comp x arr.(m) with
      | LE -> print_string "LE -> "; loop l (m-1)
      | EQ -> print_string "EQ"; Some m
      | GR -> print_string "GR -> "; loop (m+1) r
  in loop 0 (Array.length arr - 1)
;;

let f = (fun k -> k*k) ;;
bsearchF f 9 3;;

(*
invalid_arg "1" + invalid_arg "2";;
(invalid_arg "1", invalid_arg "2");;
(invalid_arg "1"; invalid_arg "2");;
(failwith "1" * failwith "2") + failwith "3";;
failwith "1" * (failwith "2" + failwith "3");;
*)
