(* n! *)

let rec fac n = 
  if n = 0 then 1
  else n * fac (n-1)
         
(* fib n *)
let rec fib n =
  if n = 0 then 0 else
  if n <= 2 then 1
  else fib (n-2) + fib (n-1)
         
(* Insertion Sort *)

let rec insert x l =
  match l with 
  | [] -> [x]
  | y :: l -> if x < y then x :: y :: l else y :: insert x l
                                               
let rec isort l =
  match l with 
  | [] -> []
  | x :: l -> insert x (isort l)
                
(* Merge Sort *)

let rec split l l1 l2 =
  match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x :: y :: l -> let (a, b) = split l [] [] in (x::a, y::b)
                                                 
let rec merge l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | [], l -> l
  | l, [] -> l
  | a :: t1, b :: t2 -> if a <= b then a :: merge t1 (b :: t2)
      else b :: merge (a :: t1) t2
             
let rec msort l =
  match l with
  | [] -> []
  | x :: l -> let (l1,l2) = split l [x] [] in merge (msort l1) (msort l2)
        
(* Quicksort *)

let swap a i j = let b = a.(i) in (a.(i) <- a.(j);a.(j) <- b; ())
                                  
let partition a l r =
  let x = a.(r) in
  let rec loop i j =
    if j < i then (swap a i r; i)
    else if a.(i) <= x then loop (i+1) j
    else if a.(j) > x then loop i (j-1)
    else (swap a i j; loop (i+1) (j-1))
  in loop 0 (r-1)
    
let qsort a =
  let x = Array.length a - 1
  in let rec qsort' l r =
       if r < l then ()
       else let m = partition a l r
         in (qsort' l (m-1); qsort' (m+1) r)
  in qsort' 0 x

(* Selection Sort *)

let ssort a =
  let r = Array.length a - 1
  in let rec min i j =
       if i > r then j
       else if a.(i) < a.(j) then min (i+1) i
       else min (i+1) j
  in let rec loop i =
       if i > r then ()
       else (swap a (min i i) i; loop (i+1))
  in loop 0
        
(* List of Sublists *)

let rec pow l =
  match l with 
  | [] -> [[]]
  | x :: l -> let r = pow l in r @ List.map (List.cons x) r
    
(* Prefixes *)                            

let rec prefixes_worker l = 
  match l with
  | [] -> [[]]
  | x :: l -> let r = prefixes_worker l in [x::l] @ r
                                           
let prefixes l = prefixes_worker (List.rev l)
    
(* Suffixes *)

let rec suffixes l =
  match l with
  | [] -> []
  | x :: l -> (x::l) :: suffixes l
                
(* Segments *)

let rec flatten l =
  match l with
  | [] -> []
  | x :: l -> x @ flatten l
                
let segments l = flatten (List.map (fun l -> prefixes l) (suffixes l) )
    
(* First *)

let rec first f n =
  if f n then n
  else first f (n+1)
      
(* Inversion of straight increasing functions *)

let rec rev_f f x =
  let k = first (fun k -> f (k+1) > x) 0 in k
    
(* Iter *)

let rec iter f n x =
  if n = 0 then x
  else if n = 1 then f x
  else f (iter f (n-1) x)
      
let fibi n = let (x,y) = iter (fun (a,b) -> (b, a+b)) n (0,1) in x
    
(* Array Reversal *) 

let arr_rev a =
  let r = Array.length a - 1
  in let rec loop i j =
       if j < i then ()
       else (swap a i j; loop (i+1) (j-1))
  in loop 0 r
      
(* fold *)

let rec foldr f l b =
  match l with
  | [] -> b
  | x :: l -> f x (foldr f l b)
                
let rec foldl f l b =
  match l with
  | [] -> b
  | x :: l -> foldl f l (f x b)
                
(* map *)

let rec map f l =
  match l with
  | [] -> []
  | x::l -> (f x) :: map f l 
