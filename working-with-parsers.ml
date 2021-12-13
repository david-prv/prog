type tree = A | B of tree * tree | C of tree * tree ;;
type token = AT | BT | CT | LP | RP ;;

let rec fold f l b =
  match l with
  | [] -> b
  | x :: t -> f x (fold f t b) ;;

let is_palindrom s =
  let len = String.length s in
  let rec explode s i a =
    if i = len then a
    else explode s (i+1) ([String.get s i] @ a)
  in let list1 = explode s 0 [] in
  let list2 = List.rev list1 in
  list1 = list2 ;;
  
(* Recursive *)  
let init f x =
  let rec init' f x a =
    if x = 0 then
      []
    else
      [f a] @ (init' f (x-1) (a+1))
  in init' f x 0 ;;
  
(* Tail Recursive *)
let init_t f n =
  let rec init' f n a =
    if n < 1 then
      a
    else
      init' f (n-1) ( [f(n-1)] @ a)
  in init' f n [] ;;
    
let explode s =
  let rec explode' s i a =
    if i = String.length s then a
    else explode' s (i+1) (a @ [String.get s i])
  in explode' s 0 [] ;;

let implode l = fold (fun c b -> String.make 1 c ^ b) l "" ;;

let verify c l = match l with
  | [] -> failwith "verify: no token"
  | c'::l -> if c'=c then l else failwith "verify: wrong token" ;;
          
let rec tree_pre l = match l with
  | LP::l -> let (t,l) = tree_pre l in (t, verify RP l) 
  | AT::l -> (A,l)
  | BT::l ->
      let (t1,l) = tree_pre l in
      let (t2,l) = tree_pre l in
      (B(t1,t2), l) 
  | _ -> failwith "tree" ;;
           
let rec tree_inf l = 
  let (t,l)= ptree l in tree' t l
and tree' t l  = match l with
  | BT::k -> let (t1,k)= ptree k in tree' (B(t,t1)) k
  | _ -> (t,l) 
and ptree l = match l with
  | AT::k -> (A,k)
  | LP::k -> let (t,k)= tree_inf k in (t, verify RP k)
  | _ -> failwith "tree" ;;
           
let rec inf t =
  let ptree t = match t with
    | A -> "A"
    | t -> "(" ^ inf t ^ ")"
  in match t with
  | A -> "A"
  | B(t1,t2) -> inf t1 ^ "B" ^ ptree t2 
  | _ -> failwith "what" ;;
           
implode ['\240'; '\159'; '\152'; '\138'] 

let lex s =
  let n = String.length s in
  let rec lex i l =
    if i >= n then List.rev l
    else match String.get s i with
      | 'A' -> lex (i+1) (AT::l)
      | 'B' -> lex (i+1) (BT::l)
      | 'C' -> lex (i+1) (CT::l)
      | '(' -> lex (i+1) (LP::l)
      | ')' -> lex (i+1) (RP::l)
      | ' ' | '\n' | '\t' -> lex (i+1) l
      | _ -> failwith "lex: illegal character"
  in lex 0 [] ;; 

let lex' s =
  let n = String.length s in
  let rec lex' i l =
    if i >= n then List.rev l
    else match String.get s i with
      | 'A' -> lex' (i+1) (AT::l)
      | 'B' -> lex' (i+1) (BT::l)
      | 'C' -> lex' (i+1) (CT::l)
      | '(' -> lex' (i+1) (LP::l)
      | ')' -> lex' (i+1) (RP::l)
      | ' ' | '\n' | '\t' | _ -> lex' (i+1) l 
  in lex' 0 [] ;; 
  
(* Skips comments too *)
let lex_ s = 
  let l = explode s in
  let rec skip l =
    match l with
    | [] -> failwith "lex: invalid syntax"
    | x :: t -> match x with
      | '*' -> begin match t with
          | [] -> failwith "lex: invalid syntax"
          | x2 :: t2 -> begin match x2 with
              | ')' -> t2
              | _ -> skip t
            end
        end
      | _ -> skip t
  in
  let rec lex_rec tl akku = 
    match tl with 
    | [] -> akku
    | x :: t -> match x with
      | 'A' -> lex_rec t (AT::akku)
      | 'B' -> lex_rec t (BT::akku)
      | 'C' -> lex_rec t (CT::akku)
      | '(' -> 
          begin
            match t with
            | x2 :: t2 ->
                begin
                  match x2 with
                  | '*' -> let new_tail = skip t2 in lex_rec new_tail akku
                  | _ -> lex_rec t (LP::akku)
                end
            | _ -> lex_rec t (LP::akku)
          end 
      | ')' -> lex_rec t (RP::akku)
      | ' ' | '\n' | '\t' | _ -> lex_rec t akku   
  in List.rev (lex_rec l [])

(* Recursive *)  
let count c s = 
  let len = String.length s in 
  let rec count' i =
    if i = len then 0 
    else let char = String.get s i in
      if char = c then 1 + count' (i+1)
      else count' (i+1)
  in count' 0 ;;    

(* Tail Recursive *)
let count' c s = 
  let n= String.length s in 
  let rec count' i c a= 
    if i>= n then a
    else match String.get s i with
      |c'-> if c'= c then count' (i+1) c (a+1)
          else count' (i+1) c a
  in
  count' 0 c 0 ;; 
                  
                  
