module type DEQUE = sig
  type 'a deque
  val make : int -> 'a -> 'a deque (* Creates a new deque *)
  val length : 'a deque -> int (* Returns the number of elements *)
  val put_front : 'a deque -> 'a -> unit (* Inserts element to the beginning *)
  val put_back : 'a deque -> 'a -> unit (* Inserts element to the back *)
  val take_front : 'a deque -> 'a (* Removes and returns the first element *)
  val take_back : 'a deque -> 'a (* Removes and returns the last element *)
end

module Dque : DEQUE = struct
  type 'a deque = ('a array * int * int) ref 
  let pos (d : 'a deque) x = let (arr,first,length) = !d in
    if x < 0 then
      (Array.length arr - x) mod Array.length arr
    else x mod Array.length arr 
  let make (n : int) (a :'a) : 'a deque = ref (Array.make n a, 0, 0)
  let length (d : 'a deque) : int = let (arr,first,length) = !d in length
  let put_back (d : 'a deque) (x : 'a) : unit =
    let (arr,first,length) = !d in let _ = (arr.(pos d (first+length)) <- x)
    in d := (arr,first,length+1)
  let put_front (d : 'a deque) (x : 'a) : unit =
    let (arr,first,length) = !d in let _ = (arr.(pos d (first-1)) <- x)
    in d := (arr,first-1,length+1)
  let take_front (d : 'a deque) = let (arr,first,length) = !d in let buff = arr.(first) in
    (d := (arr,first+1,length-1);buff)
  let take_back (d : 'a deque) = let (arr,first,length) = !d in let buff = arr.(pos d (first + length - 1)) in
    (d := (arr,first,length-1);buff)
end
