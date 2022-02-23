(* Sorting on Lists *)

(* Merge Sort : O(n)*)

let rec split l l1 l2 = 
  match l with
  | [] -> (l1,l2)
  | x::[] -> (x::l1,l2)
  | x::y::l -> let (xs,ys) = split l [] [] in (x::xs,y::ys)
;;

let rec merge l1 l2 = 
  match l1, l2 with 
  | [], [] -> [] 
  | [], l -> l
  | l, [] -> l
  | x::l1, y::l2 -> if x < y then x :: merge l1 (y::l2)
      else y :: merge (x::l1) l2
;;

let rec msort l = 
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: y :: l -> let (l1,l2) = split l [x] [y] in merge (msort l1) (msort l2)
;;


(* Generalized Insertion Sort : O(n) *)

let rec insert x p l = 
  match l with
  | [] -> [x]
  | y::l -> if p x y then x :: y :: l else y :: insert x p l 
;;

let rec gisort p l = 
  match l with
  | [] -> []
  | x :: l -> insert x p (gisort p l)
;;


(* Sorting on Arrays *)

(* Selection Sort : O(n^2) *)

let swap a x y = let buff = a.(x) in (a.(x) <- a.(y); a.(y) <- buff; ())

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

(* Quick Sort : O(n * log n) *)

let partition a l r =
  let x = a.(r) in
  let rec loop i j =
    if j<i then (swap a i r;i)
    else if a.(i) < x then loop (i+1) j
    else if a.(j) >= x then loop i (j-1)
    else (swap a i j;loop (i+1) (j-1))
  in loop l (r-1)
;;

let qsort a =
  let length = Array.length a - 1
  in let rec qsort' l r =
       if r < l then () else
         let m = partition a l r in
         (qsort' l (m-1); qsort' (m+1) r)
  in qsort' 0 length
;;
