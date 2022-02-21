
(* AB-Trees *)
type tree = A | B of tree * tree ;; 

let explode s = List.init (String.length s) (String.get s) ;;
let implode l = List.fold_right (fun c s -> String.make 1 c ^ s) l "" ;;

implode ['\240'; '\159'; '\152'; '\138'] ;;
    
(* Lex any AB/ABC-Tree Linearization to Tokenlist *)
type token = AT | BT | CT | LP | RP ;; 
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
  in lex 0 []
;;

(* Prefix - Linearization AB-Trees *)
let rec prefix t = 
  match t with
  | A -> "A"
  | B(t1, t2) -> "B" ^ prefix t1 ^ prefix t2
;;
let rec prefix_p t = 
  match t with
  | A -> "(A)"
  | B(t1, t2) -> "B(" ^ prefix_p t1 ^ prefix_p t2 ^ ")"
;;

(* Parse prefix-linearized AB-Trees *)
let rec prefixl_tree l = match l with
  | AT::l -> (A,l)
  | BT::l ->
      let (t1,l) = prefixl_tree l in
      let (t2,l) = prefixl_tree l in
      (B(t1,t2), l)
  | _ -> failwith "tree"
;;

let verify c l = match l with
  | [] -> failwith "verify: no token"
  | c'::l -> if c'=c then l else (failwith "verify: wrong token")
let rec prefixl_tree_p l = match l with
  | AT::l -> (A,l)
  | BT::l ->
      let (t1,l) = prefixl_tree_p l in
      let (t2,l) = prefixl_tree_p l in
      (B(t1,t2), l)
  | LP::l -> let (t,l) = prefixl_tree_p l in (t, verify RP l)
  | _ -> failwith "tree"
;;
(* Infix - Linearization: Fully Parenthesized *)
let rec infix t =
  match t with
  | A -> "A"
  | B(t1,t2) -> "(" ^ infix t1 ^ "B" ^ infix t2 ^ ")" 
;; 

let t1 = (B(B(A,A),B(A,A))) ;;
let pt1 = prefix t1 ;;
let pt1_lex = lex pt1 ;;

(* ABC-Trees *)
type ctree = A | B of ctree * ctree | C of ctree * ctree ;;

let ct = (C(B(A,A), C(B(A,A),B(A,A)))) ;;

(* Infix - Linearization: B before C, B and C are left-ass *)
let infix' t =
  let rec ctree t =
    match t with
    | C(t1,t2) -> ctree t1 ^ "C" ^ btree t2
    | _ -> btree t
  and btree t =
    match t with
    | B(t1,t2) -> btree t1 ^ "B" ^ atree t2
    | _ -> atree t
  and atree t =
    match t with
    | A -> "A"
    | _ -> "(" ^ ctree t ^ ")"
  in ctree t
;;

let juxta_infix' t =
  let rec ctree t =
    match t with
    | C(t1,t2) -> ctree t1 ^ "C" ^ btree t2
    | _ -> btree t
  and btree t =
    match t with
    | B(t1,t2) -> btree t1 ^ atree t2
    | _ -> atree t
  and atree t =
    match t with
    | A -> "A"
    | _ -> "(" ^ ctree t ^ ")"
  in ctree t
;;

infix' ct ;;
