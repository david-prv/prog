let l1 = [1;2;3;4;5] ;;
let l2 = [] ;;
let l3 = [3;1;5;2;4] ;; 

let max x y = if x > y then x else y ;;

let rec length l =
  match l with
  | [] -> 0
  | x :: l -> 1 + length l
;;

let rec sum l =
  match l with
  | [] -> 0
  | x :: l -> x + sum l
;;

let rec maxl l =
  match l with
  | [] -> 0
  | x :: l -> max x (maxl l)
;;

let rec map f l =
  match l with
  | [] -> []
  | x :: l -> (f x) :: map f l
;;

let rec app a b =
  match a with
  | [] -> b
  | x :: l -> x :: app l b
;;

let rec flatten l a =
  match l with
  | [] -> a
  | x :: t -> flatten t (a @ x)
;;

let rec nth l c =
  match l with
  | [] -> failwith "Index out of range"
  | x :: l -> if c = 0 then x else nth l (c-1)
;;

let rec rev l =
  match l with 
  | [] -> []
  | x :: l -> (rev l) @ [x]
;;

let rec part l m n =
  match l with
  | [] -> failwith "Partition out of range"
  | x :: l -> if m = 0 then
        if n = 0 then [] else  x :: part l m (n-1)
      else part l (m-1) n
;;

let rec seq m n =
  if m > n then
    []
  else
    m :: (seq (m+1) n)
;;

let rec seq' m n =
  if n < m then
    []
  else
    (seq' m (n-1)) @ [n]
;;

let rec pseq f x y =
  if x > y then
    []
  else
    (f x) :: (pseq f (x+1) y)
;;

let pseq' f x y = map f (seq x y) ;;

let rec count x l =
  match l with
  | [] -> 0
  | y :: l -> if y = x then 1 + count x l else count x l
;;

let rec filter f l =
  match l with
  | [] -> []
  | x :: l -> if f x then x :: filter f l else filter f l
;;

let rec last l =
  match l with
  | x :: [] -> x
  | x :: l -> last l
  | _ -> failwith "Empty list"
;;

let rec find x l =
  match l with
  | [] -> failwith "Not found"
  | y :: l -> if x = y then 0 else 1 + find x l
;;

let rec forall f l =
  match l with
  | [] -> true
  | x :: l -> (f x) && forall f l
;;

let rec exists f l =
  match l with
  | [] -> false
  | x :: l -> (f x) || exists f l
;;

let rec prefixes l =
  match l with
  | [] -> [[]]
  | x :: l -> let t = prefixes l in t @ List.map (List.cons x) t
;;

let rec suffixes l =
  match l with
  | [] -> [[]]
  | x :: t -> [l] @ suffixes t
;;

let segments l = flatten (List.map (fun l -> suffixes l) (prefixes l)) [] ;;

let rec fold f l b =
  match l with
  | [] -> b
  | x :: l -> f x (fold f l b)
;;

let rec foldl f l b =
  match l with
  | [] -> b
  | x :: l -> foldl f l (f x b)
;;
