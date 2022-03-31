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
  | B(t1,t2)-> 1 + max (depth t1) (depth t2)
                  
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
           
let bool2string b =
  match b with
  | true -> "true"
  | false -> "false"

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
  
let cstyle t =
  let rec cstyle' t =
    match t with
    | A -> "A"
    | B(t1,t2) -> "B(" ^ cstyle' t1 ^ "," ^ cstyle' t2 ^ ")"
  in let str = cstyle' t in
  "(" ^ str ^ ")"
  
let rec pre t =
  match t with
  | A -> "A"
  | B(t1,t2) -> "B" ^ pre t1 ^ pre t2 
                  
let rec post t = 
  match t with
  | A -> "A"
  | B(t1,t2) -> post t1 ^ post t2 ^ "B" 
 
let rec inf t =
  let ptree t = match t with
    | A -> "A"
    | t -> "(" ^ inf t ^ ")"
  in match t with
  | A -> "A"
  | B(t1,t2) -> inf t1 ^ "B" ^ ptree t2
                  
let rec inf_f t =
  let ptree t = match t with
    | A -> "A"
    | t -> "(" ^ inf_f t ^ ")"
  in match t with
  | A -> "A"
  | B(t1,t2) -> "(" ^ inf_f t1 ^ "B" ^ ptree t2 ^ ")"  
                  
type ctree = A | B of ctree * ctree | C of ctree * ctree                  
        
let rec pre_abc t =
  match t with
  | A -> "A" 
  | B(t1,t2) -> "B" ^ pre_abc t1 ^ pre_abc t2
  | C(t1,t2) -> "C" ^ pre_abc t1 ^ pre_abc t2
                  
let rec post_abc t = 
  match t with
  | A -> "A"
  | B(t1,t2) -> post_abc t1 ^ post_abc t2 ^ "B"
  | C(t1,t2) -> post_abc t1 ^ post_abc t2 ^ "C"
                
let rec inf_f_abc t =
  let ptree t = match t with
    | A -> "A"
    | t -> "(" ^ inf_f_abc t ^ ")"
  in match t with
  | A -> "A"
  | B(t1,t2) -> "(" ^ inf_f_abc t1 ^ "B" ^ ptree t2 ^ ")"  
  | C(t1,t2) -> "(" ^ inf_f_abc t1 ^ "C" ^ ptree t2 ^ ")"  
        
(****************************)
(* brainstorming, not my work *)
        
(*C left, B left*)
let rec ctree t = match t with
  |C(t1, t2)-> ctree t1 ^"C" ^btree t2
  |t-> btree t
and btree t = match t with
  |B(t1,t2)-> btree t1 ^"B"^ptree t2
  |t->ptree t
and ptree t = match t with
  |A-> "A"
  |t-> "(" ^ctree t^")"
        
(*B right, C right*)
let rec ctreei t = match t with
  |C(t1,t2)-> btree t1 ^"C" ^ ctreei t2
  |t-> btree t
and btree t = match t with
  |B(t1,t2)-> ptree t1 ^ "B" ^ btree t2
  |t->ptree t
and ptree t = match t with
  |A-> "A"
  |t-> "(" ^ctreei t ^")"
       
(*B left, C right*)
let rec ctreeii t= match t with 
  |C(t1,t2)-> btree t1 ^"C" ^ ctreeii t2
  |t-> btree t
and btree t = match t with
  |B(t1,t2)-> btree t1 ^ "B" ^ ptree t2
  |t->ptree t
and ptree t = match t with
  |A-> "A"
  |t-> "(" ^ctreeii t ^")"   
       
(*B right, C left*)
let rec ctreerl t = match t with
  |C(t1, t2)-> ctreerl t1 ^"C" ^btree t2
  |t-> btree t
and btree t = match t with
  |B(t1,t2)-> ptree t1 ^"B"^btree t2
  |t->ptree t
and ptree t = match t with
  |A-> "A"
  |t-> "(" ^ctreerl t^")"
  
(****************************)
       
(* Abstract expressions *)

type var = string
type con = Bcon of bool | Icon of int
type op  = Add | Sub | Mul | Leq
type exp = Var of var | Con of con
         | Oapp of op * exp * exp
         | Fapp of exp * exp
         | If of exp * exp * exp
         | Lam of var * exp
         | Let of var * exp * exp
         | Letrec of var * var * exp * exp 
                     
let expFact = Letrec("fact", "x", If(Oapp(Leq, Var "x", Con (Icon 1)), Con (Icon 1), Oapp(Mul, Var "x", Fapp(Var "fact", Oapp(Sub, Var "x", Con (Icon 1))))), Fapp(Var "fact", Con (Icon 10)))

(* Algorithmic reading of abstract expressions *)

let rec mem x l =
  match l with
  | [] -> false
  | y :: l -> (x = y) || mem x l

let rec algo_read env exp =
  match exp with
  | Con(con) -> true
  | Var(var) -> mem var env
  | Oapp(op,ex1,ex2) -> (algo_read env ex1) && (algo_read env ex2)
  | Fapp(ex1,ex2) -> (algo_read env ex1) && (algo_read env ex2)
  | If(ex1,ex2,ex3) -> (algo_read env ex1) && (algo_read env ex2) && (algo_read env ex3)
  | Lam(var,ex) -> algo_read (env @ [var]) ex
  | Let(var,ex1,ex2) -> (algo_read env ex1) && (algo_read (env @ [var]) ex2)
  | Letrec(var1,var2,ex1,ex2) -> (algo_read ((env @ [var1]) @ [var2]) ex1) && (algo_read (env @ [var1]) ex2)
                                           
(* Check for closeness in expressions *)

let is_exp_closed exp =
  match (algo_read [] exp) with
  | true -> true
  | _ -> false
    
(* Return free variables of expression *)

let free_vars exp =
  let akku = [] in
  let free_vars' env exp =
    let rec free x = match akku @ [x] with
      | _ -> false
    in match exp with
    | Con(con) -> true
    | Var(var) -> if mem var env then true else free var
    | Oapp(op,ex1,ex2) -> (algo_read env ex1) && (algo_read env ex2)
    | Fapp(ex1,ex2) -> (algo_read env ex1) && (algo_read env ex2)
    | If(ex1,ex2,ex3) -> (algo_read env ex1) && (algo_read env ex2) && (algo_read env ex3)
    | Lam(var,ex) -> algo_read (env @ [var]) ex
    | Let(var,ex1,ex2) -> (algo_read env ex1) && (algo_read (env @ [var]) ex2)
    | Letrec(var1,var2,ex1,ex2) -> (algo_read ((env @ [var1]) @ [var2]) ex1) && (algo_read (env @ [var1]) ex2)
  in let _ = free_vars' [] exp
  in akku
                                                                              
  
(* Linearization of abstract expressions *)

let rec exp_lin exp =
  match exp with 
  | Var(var) -> var
  | Oapp(op,ex1,ex2) -> cexp exp
  | Fapp(ex1,ex2) -> exp_lin ex1 ^ " " ^ exp_lin ex2
  | If(ex1,ex2,ex3) -> "if " ^ exp_lin ex1 ^ " then " ^ exp_lin ex2 ^ " else " ^ exp_lin ex3
  | Lam(var,ex) -> "fun " ^ var ^ " -> " ^ exp_lin ex
  | Let(var,ex1,ex2) -> "let " ^ var ^ " = " ^ exp_lin ex1 ^ " in " ^ exp_lin ex2
  | Letrec(var1,var2,ex1,ex2) -> "let rec " ^ var1 ^ " " ^ var2 ^ " = " ^ exp_lin ex1 ^ " in " ^ exp_lin ex2
  | Con(con) -> match con with
    | Icon(int) -> digit2string int
    | Bcon(bool) -> bool2string bool 
and cexp exp =
  match exp with
  | Oapp(Leq,ex1,ex2) -> sexp ex1 ^ " <= " ^ sexp ex2
  | _ -> sexp exp
and sexp exp =
  match exp with
  | Oapp(Add,ex1,ex2) -> sexp ex1 ^ " + " ^ sexp ex2
  | Oapp(Sub,ex1,ex2) -> sexp ex1 ^ " - " ^ sexp ex2
  | _ -> mexp exp
and mexp exp =
  match exp with
  | Oapp(Mul,ex1,ex2) -> mexp ex1 ^ " * " ^ aexp ex2
  | _ -> aexp exp
and aexp exp =
  match exp with
  | Fapp(ex1,ex2) -> (aexp ex1) ^ " " ^ (pexp ex2)
  | _ -> pexp exp
and pexp exp =
  match exp with
  | Var(var) -> var
  | Con(con) -> begin match con with | Bcon(bool) -> bool2string bool | Icon(int) -> digit2string int end
  | exp -> "(" ^ exp_lin exp ^ ")"
                      
