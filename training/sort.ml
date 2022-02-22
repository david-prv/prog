let rec split l l1 l2 = match l with
  | [] -> (l1,l2)
  | [x] -> (x::l1,l2)
  | x::y::l -> split l (x::l1) (y::l2)
let rec merge l1 l2 = match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | x::l1, y::l2 when x <= y -> x :: merge l1 (y::l2)
  | x::l1, y::l2 -> y :: merge (x::l1) l2
let rec msort l = match l with
  | x::y::l -> let (l1,l2) = split l [x] [y] in merge (msort l1) (msort l2) 
  | l -> l

let swap a i j = let buff = a.(i) in (a.(i) <- a.(j);a.(j) <- buff)
                                     
let partition a l r =
  let x = a.(r) in
  let rec loop i j =
    if j < i
    then (swap a i r; i)
    else if a.(i) < x then loop (i+1) j
    else if a.(j) >= x then loop i (j-1)
    else (swap a i j; loop (i+1) (j-1))
  in loop l (r-1)
