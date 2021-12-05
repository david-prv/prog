let succ x = x + 1 
let fst (x,y) = x
let fst' (x,y,z) = x
let snd (x,y) = y
  
let rec first f n =
  if f n then n
  else first f (n+1)

let rec iter f n x =
  if n < 1 then x
  else iter f (n-1) (f x)
      
let add x y = iter succ y x
let mult x y = iter (add y) x 0
let pow x n = iter (mult x) n 1
let sub x y = iter pred y x
    
let zwei_a x = x
let zwei_b x y = iter (add y) x 0
let zwei_c f a b = f (a,b)
let zwei_d f (a,b) = f a b
    
let lambda = (fun (x,y) -> fun f -> (y, f x))
             
let pred n = fst(iter(fun (a,k) -> (k, succ k)) n (0,0))
    
let sub' x y = let a = sub x y in
  if a < 0 then 0
  else a
    
let sum x = fst(iter(fun (a,k) -> (add a k, succ k)) x (0,1))
let sum' f x = fst(iter(fun (a,k) -> (add a (f k), succ k)) x (0,1))
    
let fac n = fst(iter(fun (a,k) -> (mult a k, succ k)) n (1,1))
let sequence_of_3_7_adds x n = fst(iter(fun (a,k) -> (add a k, if k = 3 then 7 else 3)) n (x,3))
let trib x = fst'(iter(fun (a,b,c) -> (b, c, a+b+c)) x (0,0,1))
let forall m n f = fst(iter(fun (a, k) -> (a && (f k), succ k)) (n-m+1) (true,m))
let exists m n f = fst(iter(fun (a, k) -> (a || (f k), succ k)) (n-m+1) (false,m))
    
(* Fib recursive without iter *)
let rec fib n=
  if n <= 2 then 1
  else fib (n-2)  + fib (n-1)
  
(* Fib tail-recursive without iter *)       
let rec fib n a1 a2 =
  if n < 2 then a2
  else fib (n-1) (a2) (a1+a2)
      
let prime_test x = (x >=2) && ((first (fun k -> x mod k = 0) 2) = x)
let prime_test' x = let a = (x >= 2) in
  let b = (first(fun k -> (k*k >= x) || (x mod k = 0)) 2) in
  a && (b*b > x)
       
let next_prime x = first prime_test (x+1)
let nth_prime n = iter next_prime n 2
let nth_prime_sqr n = let a = (iter next_prime n 2) in a*a
                                                       
(* iterating on booleans *)
let even n = iter (fun a -> not a) n true 
