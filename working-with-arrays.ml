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
    
let reverse a : unit =
  let rec loop i j =
    if i > j then ()
    else (swap a i j; loop (i+1) (j-1))
  in loop 0 (Array.length a - 1)

let test = invalid_arg "1" + invalid_arg "2";;
let test = (invalid_arg "1", invalid_arg "2");;
let test = (invalid_arg "1"; invalid_arg "2");;
    
(failwith "1" * failwith "2") + failwith "3";;
failwith "1" * (failwith "2" + failwith "3");;
