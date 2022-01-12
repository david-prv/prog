let rec append l1 l2 = match l1 with
  | [] -> l2
  | x :: l1 -> x :: append l1 l2 
let rec rev l = match l with
  | [] -> []
  | x :: l -> append (rev l) [x]
let rec rev_append l1 l2 = match l1 with
  | [] -> l2
  | x :: l1 -> rev_append l1 (x::l2)
let rec prefixes l = match l with
  | [] -> [[]]
  | x :: l -> [] :: List.map (List.cons x) (prefixes l)
 
 (* linear running time reverse alternative *)
let rec rev' l =
  match l with
  | [] -> []
  | x::l -> rev_append l [x]
           
(* function that generates lists of length n *)
let rec f n =
  match n with
  | 0 -> []
  | x -> x :: f (n-1)

(* Merge Sort declarations *)
let rec append l1 l2 = match l1 with
  | [] -> l2
  | x :: l1 -> x :: append l1 l2 
let rec rev l = match l with
  | [] -> []
  | x :: l -> append (rev l) [x]
let rec rev_append l1 l2 = match l1 with
  | [] -> l2
  | x :: l1 -> rev_append l1 (x::l2)
let rec prefixes l = match l with
  | [] -> [[]]
  | x :: l -> [] :: List.map (List.cons x) (prefixes l)
  
let rec rev' l =
  match l with
  | [] -> []
  | x::l -> rev_append l [x]
              
let rec f n =
  match n with
  | 0 -> []
  | x -> x :: f (n-1)
              
