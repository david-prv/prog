let rec partition a l r =
  let x = a.(r) in
  let rec loop i j =
    if j < i then (swap a i r;i)
    else if a.(i) < x then loop (i+1) j
    else if a.(j) >= x then loop i (j-1)
    else (swap a i j; loop (i+1) (j-1))
  in loop l (r-1)
  
let qsort a =
  let rec qsort' l r =
    if r < l then ()
    else let m = partition a l r in
      (qsort' l (m-1); qsort' (m+1) r)
  in qsort' 0 (Array.length a - 1)
    
let rec split l l1 l2 =
  match l with 
  | [] -> (l1,l2)
  | x :: y :: l -> split l (x::l1) (y::l2)
  | x :: [] -> split l (x::l1) l2
                 
let rec merge l1 l2 =
  match l1,l2 with
  | [], [] -> []
  | x :: l, [] -> (x::l)
  | [], x :: l -> (x::l)
  | x::l1, y::l2 -> if x < y then x :: merge l1 (y::l2) else y :: merge (x::l1) l2
                                                               
let rec msort l =
  match l with 
  | x::y::l -> let (l1,l2) = split l [x] [] in merge (msort l1) (msort l2)
  | x::[] -> [x]
  | [] -> []
