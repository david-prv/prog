module type CELL = sig
  type 'a cell
  val make : 'a -> 'a cell
  val get : 'a cell -> 'a
  val set : 'a cell -> 'a -> unit
end

module type STACK = sig
  type 'a stack
  val make : 'a -> 'a stack
  val push : 'a stack -> 'a -> unit
  val pop : 'a stack -> unit
  val top : 'a stack -> 'a
  val height : 'a stack -> int
end

module type QUEUE = sig
  type 'a queue
  val make : 'a -> 'a queue
  val insert : 'a queue -> 'a -> unit
  val remove : 'a queue -> unit
  val first : 'a queue -> 'a
  val length : 'a queue -> int
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

module Stack : STACK = struct
  type 'a stack = 'a list ref
  exception Empty
  let make x = ref [x]
  let push s x = s:= x :: !s
  let pop s = match !s with
    | [] -> raise Empty
    | x::l -> s:= l
  let top s = match !s with
    | [] -> raise Empty
    | x::l ->x
  let height s = List.length (!s)
end 

module Queue : QUEUE = struct
  type 'a queue = 'a list ref
  exception Empty
  let make x = ref [x]
  let insert q x = q := !q @ [x]
  let remove q = match !q with
    | [] -> raise Empty
    | _::l -> q := l
  let first q = match !q with
    | [] -> raise Empty
    | x::_ -> x
  let length q = List.length (!q)
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

let enum =
  let c = Cell.make 0
  in fun () -> let x = Cell.get c in
    Cell.set c (x+1); x 
