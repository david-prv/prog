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
  
  
