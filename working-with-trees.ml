type tree = A | B of tree * tree

(* Example declaration of trees *)                     
let t = B(B(A,A), B(B(A,A),A)) 
    
let t1 = B(A,B(A,A))
let t2 = B(B(A,A),B(A,A))
let t3 = B(B(A,B(A,A)),A)
    
let rec size t = 
  match t with
  | A -> 1
  | B(t1,t2) -> 1+size(t1)+size(t2)
                  
let rec breadth t = 
  match t with
  | A -> 1
  | B(t1,t2) -> breadth(t1)+breadth(t2)
          
let max x y = if x < y then y else x                  
        
let rec depth t =
  match t with
  | A -> 0
  | B(t1,t2) -> 1 + max (depth t1) (depth t2)
                  
let rec mirror t = match t with
  |A -> A
  |B(t1,t2) -> B(mirror t2, mirror t1)
                 
let rec deepTree n =
  if n = 0 then
    A
  else
    B(deepTree (n-1), A)
      
let rec btree n =
  if n = 0 then
    A
  else
    let t = btree(n-1) in B(t, t)
      
let rec balanced t =
  match t with
  | A -> Some 0
  | B(t1,t2) -> match balanced t1, balanced t2 with
    | Some x, Some y -> if x = y then
          Some (1 + x)
        else
          None
    | _, _ -> None
      
let slinb l = 
  let rec slinb' l =
    match l with
    | x :: [] -> if x = true then "true" else "false"
    | x :: t -> (slinb' [x]) ^ ";" ^ (slinb' t)
    | _ -> ""
  in let a = slinb' l
  in "[" ^ a ^ "]"
     
let digit2string d =
  match d with
  | 1 -> "1" | 4 -> "4" | 7 -> "7"
  | 2 -> "2" | 5 -> "5" | 8 -> "8"
  | 3 -> "3" | 6 -> "6" | 9 -> "9"
  | 0 -> "0" | _ -> ""
           
let prefix n =
  if n < 0 then
    "-"
  else
    ""

let slinn n =
  let num = if n < 0 then n * (-1) else n
  in let rec slinn' n = 
       if n < 10 then
         digit2string n
       else
         slinn' (n/10) ^ slinn' (n mod 10)
  in let str = slinn' num
  in (prefix n) ^ str
     
let slini l = 
  let rec slini' l =
    match l with
    | x :: [] -> slinn x
    | x :: t -> (slini' [x]) ^ ";" ^ (slini' t)
    | _ -> ""
  in let a = slini' l
  in "[" ^ a ^ "]"
