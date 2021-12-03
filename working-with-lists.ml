let rec first f k = if f k then k else first f (k+1)

let rec length l = 
  match l with
  | [] -> 0
  | x :: l -> 1 + length l
                
let rec null l =
  match l with
  | [] -> true
  | x :: l -> false
    
let rec conc l1 l2 =
  match l1 with
  | [] -> l2
  | x :: l1 -> x :: (conc l1 l2)
                    
let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: l1 -> rev_append l1 (x :: l2) 
                 
let rec rev l =
  match l with
  | [] -> [] 
  | x :: l -> conc (rev l) [x]
                 
let rec map f l =
  match l with
  | [] -> []
  | x :: l -> f x :: map f l
                
let rec dec n =
  if n < 10 then
    [n]
  else
    conc (dec (n / 10)) [(n mod 10)] 
      
let get_head l =
  match l with
  | [] -> failwith "hd"
  | x :: l -> x
    
let get_tail l =
  match l with
  | [] -> failwith "tl"
  | x :: l -> l
    
let rec num l akku =
  if (length l) = 0 then
    akku
  else
    num (get_tail l) ((akku * 10) + (get_head l))
      
let rec num' l akku =
  match l with
  | [] -> akku
  | x :: l -> num l ((akku * 10) + x) 

let rec mem x l =
  match l with
  | [] -> false
  | y :: l -> (x = y) || mem x l
                
let rec exists f l =
  match l with
  | [] -> false
  | x :: l -> (f x) || (exists f l)
                       
let mem' x l = exists (fun k -> k = x) l    
                       
let rec forall f l =
  match l with
  | [] -> true
  | x :: l -> (f x) && (forall f l)
                       
let rec count x l =
  match l with
  | [] -> 0
  | y :: l -> if x == y then 1 + count x l else count x l
          
let rec incl l1 l2 =
  match l1 with
  | [] -> true
  | x :: l -> if (count x l2) > 0 then incl l l2 else false
        
let rec nth l n =
  match l with
  | [] -> failwith "nth"
  | x :: l -> if n < 1 then x else nth l (n-1)
          
          
let rec nth_opt l n =
  match l with
  | [] -> None
  | x :: l -> if n < 1 then Some x else nth_opt l (n-1)
          
let rec find x l =
  match l with
  | [] -> failwith "find"
  | y :: l -> if (y = x) then 0 else (1 + (find x l))
                                     
let rec find_opt x l a =
  match l with
  | [] -> None
  | y :: l -> if y = x then Some a else find_opt x l (a+1)
          
let rec find_last_opt x l a1 a2 =
  match l with
  | [] -> if a2 > 0 then Some a2 else None
  | h :: t -> if h = x then find_last_opt x t (a1+1) a1 else find_last_opt x t (a1+1) a2
  
let rec eq (p: 'a -> 'a -> bool) l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x::l1, y::l2 -> p x y && eq p l1 l2
  | _, _ -> false
  
let swap l = 
  match l with
  | (x,y) -> (y,x) 
             
let test l =
  match l with
  | [] -> false
  | x :: l -> x = 1 &&
              match l with
              | [] -> false
              | y :: l -> y = 2
                          
let test' l = match l with
  | 0::x::_-> Some x
  | x::1::_ -> Some x
  | _ -> None
    
let rec power l =
  match l with 
  | [ ] -> [[ ]] 
  | x::l -> power l @ List.map (List.cons x) (power l)
              
let rec lmax l a =
  match l with
  | [] -> a
  | x :: l -> lmax l (if a < x then x else a)
      
let rec flatten l =
  match l with
  | [] -> []
  | x :: t -> conc x (flatten t) 
                
let rec seq m n = 
  if m > n then
    []
  else
    m :: (seq (m+1) n) 
         
let rec hseq' m n =
  if n < m then
    []
  else
    n :: (hseq' m (n-1))
  
let seq' m n = rev(hseq' m n)
    
let rec filter f l =
  match l with
  | [] -> []
  | x :: l -> if (f x) then x :: (filter f l) else (filter f l)
                    
let test'' l =
  match l with
  | [] -> false 
  | h :: t -> h = 1 && 
              match t with 
              | [] -> false 
              | g :: s -> g = g && 
                          match s with
                          | [] -> false 
                          | i :: u -> i = 2;; 

let test''' l =
  match l with
  | [] -> None 
  | x::l -> match l with 
    | [] -> None
    | s::g -> if x = 0 then Some s else if s = 1 then Some x else None
          
let rec prefix l = match l with
  | [] -> [[]]
  | x::l -> [[x]] @ map (List.cons x) (prefix l)        
              
let split l =
  let rec splits l l' = match l with
    | x :: l -> (x::l,l') :: splits l (l' @ [x])
    | [] -> [([]), l']
  in splits l []
    
let rec unzip l a b =
  match l with
  | [] -> (a, b)
  | (x,y) :: l -> unzip l (a @ [x]) (b @ [y])    
                    
let rec unzip' l =
  match l with
  | (x , y ) :: l ->
      let ( xs , ys ) = unzip' l in
      ( x :: xs , y :: ys )
  | [] -> ([] ,[])

let rec zip l1 l2 =
  match l1, l2 with 
  | x1 :: tx, y1 :: ty -> [(x1, y1)] @ zip tx ty
  | _, _ -> [] 
            
let ifs x f g =
  match x with
  | x when (x < 3) -> 3
  | _ -> match f x with
    | true -> g x
    | false -> 10
                            
let rec fold f l b= match l with
  |[]-> b
  |x::l-> f x (fold f l b)
   
let length l = fold (fun a b-> b +1) l 0
let sum l= fold (fun a b-> b+a   ) l 0
let lmax l = fold (fun a b-> if a > b then a else b) l 0
let app l1 l2 = fold (fun a b -> a::b) l1 l2
let flatten l= fold ( app ) l []
let map f l= fold (fun a b-> f a ::b) l []
let nth l n = fold (fun a b-> if a= n then 0 else 1+ b) l 0
let rev l= fold (fun a b -> b @ [a]) l []
let mem x l= fold (fun a b -> if a= x then true else b ) l false 
    
let rec sorted (l : int list) =
  match l with
  | [] -> true 
  | x :: l -> match l with
    | [] -> true
    | y :: t -> if x <= y then true && (sorted t) else false
          
let rec sorted' f (l : int list) =
  match l with
  | [] -> true 
  | x :: l -> match l with
    | [] -> true
    | y :: t -> if f x y then true && (sorted' f t) else false
          
let rec sorted_t' f (l : int list) =
  match l with
  | [] -> true 
  | x :: l -> match l with
    | [] -> true
    | y :: t -> if f x y then sorted_t' f t else false
          
let rec insert x l= match l with 
  |[]-> [x]
  |y::l-> if x<= y then x::y::l else y:: insert x l
                                       
let rec sort l =match l with
  |[]->[]
  |x::l->match l with
    |[]-> [x]
    |y::k-> if x<=y then insert x (sort l) else insert x (sort l)
            
let is_sublist l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | _, _ -> if not ((find_opt l1 (power l2) 0) = None) then true else false
            
let rec remove x l =
  match l with
  | [] -> []
  | h :: t -> if not (h = x) then h :: (remove x t) else t
        
let rec undup l a =
  match l with
  | [] -> a
  | x :: t -> if (is_sublist [x] a) then undup t a else undup t (a @ [x])
          
let smallest_factor n = first  ( fun k -> n mod k = 0) 2 
    
let rec prime_fac x = if x<2 then [] 
  else let k= first (fun k-> x mod k =0) 2 in
    if k=x then [x] else k:: prime_fac (x/k)
                           
let rec lex p l1 l2 = match l1 , l2 with 
  |[], l2-> true
  |x::l1, [] -> false
  |x::l1, y::l2 -> p x y && if p y x then lex p l1 l2 else true 
                     
let rec lookup l a =
  match l with
  | [] -> None
  | (a',b)::l -> if a' = a then Some b else lookup l a 
    
let rec lookup_for_multiple_keys l a =
  match l with
  | [] -> []
  | (a',b)::l -> if a' = a then
        Some b :: lookup_for_multiple_keys l a
      else
        lookup_for_multiple_keys l a           
      
let rec lookup' l a =
  match l with
  | [] -> 0
  | (a',b)::l -> if a' = a then b else lookup' l a 
          
let rec update l a b =
  match l with
  | [] -> [(a,b)]
  | (a',b') :: l -> if a' = a then (a,b) :: l else (a',b')::update l a b
                                                     
let empty_env l =
  match l with
  | [] -> true
  | _ -> false
    
let emtpy_env' =
  []
    
let rec remove l a =
  match l with
  | [] -> []
  | (a',b') :: l -> if a' = a then remove l a else (a',b') :: remove l a
                                                     
let rec adjust l a f =
  match l with
  | [] -> []
  | (a',b') :: l -> if a' = a then (a', (f b')) :: adjust l a f else (a',b') :: adjust l a f
                                                                       
let rec elementCount l a =
  match l with
  | [] -> a
  | h :: t -> if empty_env a then 
        elementCount t [(h,1)]
      else 
        elementCount t (update a h ((lookup' a h) + 1))
          
let rec prime_fac x =
  if x < 2 then []
  else let k = first (fun k -> x mod k = 0) 2 in
    if k = x then [x]
    else k :: prime_fac (x / k)
           
let rec prime_fac' k x =
  if k * k > x then [x]
  else if x mod k = 0 then k :: prime_fac' k (x / k)
  else prime_fac' (k + 1) x
                
