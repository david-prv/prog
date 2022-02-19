type tree = A | B of tree * tree
                     
let t1 = B(A,B(A,A)) ;;
let t2 = B(B(A,A),B(A,A)) ;;
let t3 = B(B(A,B(A,A)),A) ;;

let max x y = if x > y then x else y ;;

let rec size t =
  match t with
  | A -> 1
  | B(t1,t2) -> 1 + (size t1) + (size t2)
;;

let rec depth t =
  match t with
  | A -> 0
  | B(t1,t2) -> 1 + max (depth t1) (depth t2)
;;

let rec breadth t =
  match t with
  | A -> 1
  | B(t1,t2) -> (breadth t1) + (breadth t2)
;;

let rec mirror t =
  match t with
  | A -> A
  | B(t1,t2) -> B(mirror t2, mirror t1)
;;

let rec dtree n =
  if n = 0 then A
  else B( dtree (n-1), A)
;;

let rec btree n =
  if n = 0 then A
  else let t = btree (n-1) in B(t,t)
;;

let rec bal t =
  match t with
  | A -> Some 0
  | B(t1, t2) -> match bal t1, bal t2 with
    | Some n1, Some n2 -> if n1 = n2 then Some (n1 + 1) else None
    | _, _ -> None
;;
