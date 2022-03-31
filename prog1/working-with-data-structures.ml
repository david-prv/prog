type tree = A | B of tree * tree

module type HEAP = sig
  type address = int
  type index = int
  val alloc : int -> address
  val get : address -> index -> int
  val set : address -> index -> int -> unit
  val release : address -> unit
end 


module H : HEAP = struct
  let maxSize = 1000
  let h = Array.make maxSize (-1)
  let s = ref 0 (* current size of heap *)
  exception Address
  exception Full
  type address = int
  type index = int
  let alloc n = if n < 1 then raise Address
    else if !s + n > maxSize then raise Full
    else let a = !s in s:= !s + n; a
  let check a = if a < 0 || a >= !s then raise Address else a
  let get a i = h.(check(a+i))
  let set a i x = h.(check(a+i)) <-x
  let release a = s:= check a
end

module type CELL = sig
  type 'a cell
  val make : 'a -> 'a cell
  val get : 'a cell -> 'a
  val set : 'a cell -> 'a -> unit
end

module type BSTACK = sig
  val empty : unit -> bool
  val full : unit -> bool
  val push : int -> unit
  val pop : unit -> unit
  val top : unit -> int
end 

module Cell : CELL = struct
  type 'a cell = 'a array
  let make x = Array.make 1 x
  let get c = c.(0)
  let set c x = c.(0) <- x
end 

module S : BSTACK = struct
  let size = 100
  let a = Array.make size 0
  let h = ref 0 (* height of the stack *)
  exception Empty
  exception Full
  let empty () = !h = 0
  let full () = !h = size
  let push x = if full() then raise Full else (a.(!h) <- x; h:= !h + 1)
  let pop () = if empty() then raise Empty else h:= !h - 1
  let top () = if empty() then raise Empty else a.(!h -1)
end

(* Variant of List-based Stack, where max height can be passed as argument of HStack.make *)
module type HSTACK = sig
  type 'a stack
  val make : int -> 'a -> 'a stack
  val push : 'a stack -> 'a -> unit
  val pop : 'a stack -> unit
  val top : 'a stack -> 'a
  val height : 'a stack -> int
  val max : 'a stack -> int
end

module HStack : HSTACK = struct
  let h = ref 0
  type 'a stack = 'a list ref
  exception Empty
  exception Full
  let make max x = let _ = h := max in ref [x] 
  let height s = List.length (!s)
  let max s = !h 
  let push s x = if height s = max s then raise Full else s:= x :: !s
  let pop s = match !s with
    | [] -> raise Empty
    | _::l -> s:= l
  let top s = match !s with
    | [] -> raise Empty
    | x::_ -> x 
end

let enum =
  let c = Cell.make 0
  in fun () -> let x = Cell.get c in
    Cell.set c (x+1); x 
;; 

let enum_squares =
  let c = Cell.make 0
  in fun () -> let x = Cell.get c in
    Cell.set c (x+1); x*x 
;;

let enum_fib =
  let c1 = Cell.make 0 in
  let c2 = Cell.make 1
  in fun () -> let x = Cell.get c1 in
    let y = Cell.get c2 in
    Cell.set c2 x; Cell.set c1 (x+y); Cell.get c1 
;; 

let newCounter () = (* unit means no meaningful input *)
  let enum =
    let c = Cell.make 0
    in fun () -> let x = Cell.get c in
      Cell.set c (x+1); x 
  in enum
;; 

let tabulate (n : int) (f : unit -> 'a) =
  let rec tab' n f a =
    if n = 0 then a
    else tab' (n-1) f (a @ [f()])
  in tab' n f []
;; 

let rec toList () = 
  let rec loop a =
    if S.empty () then a
    else begin
      let topmost = S.top () in
      let _ = S.pop () in
      loop (a @ [topmost])
    end
  in loop []
;;
  
let ofList l =
  let rec loop l = 
    match l with
    | [] -> ()
    | x::l -> let _ = loop l in S.push x
  in loop l
;;
  
let alloc' l =
  let a = H.alloc (List.length l) in
  let rec loop l i = match l with
    | [] -> a
    | x::l -> H.set a i x; loop l (i+1)
  in loop l 0
;;

let rec putlist l = match l with
  | [] -> -1
  | x::l -> alloc' [x; putlist l]
;;

let rec putlist' l a = match l with
  | [] -> a
  | x :: l -> putlist' l (alloc' (x::a::[]))
;;

let rec getlist a =
  if a = -1 then []
  else H.get a 0 :: getlist (H.get a 1)
;;

let rec getlist' a akku =
  if a = -1 then akku
  else getlist' (H.get a 1) (H.get a 0 :: akku)
;;

let alloc_cl n =
  let fst = ref 0 in
  let rec loop i a =
    let _ = if i = 2 then fst := a else fst := !fst in
    if i > n then let _ = H.set a 1 !fst in a
    else loop (i+1) (alloc' [i;a])
  in loop 1 (-1)
;;

let rec puttree t = match t with
  | A -> -1
  | B(t1,t2) -> alloc' [puttree t1; puttree t2]
;;

let rec gettree a =
  if a = -1 then A
  else B(gettree (H.get a 0), gettree (H.get a 1))
;;
      
let exponential (b:int) = 
  let c = ref (1,b) in
  let n = ref 0 in
  fun () -> if !n = 0 then let _ = n:= !n + 1 in 1
    else let _ = n := !n + 1 in let (x,y) = !c in (c:= (x * y, b); x * y)
;;

let exp b = let c = ref 1 in
  fun () -> let x = !c in (c := x * b; x)
;;

let tribs = let c = ref (0,1,1) in
  fun () -> let (a,b,c) = !c in (c := (b,c,a+b+c);a)
;;
