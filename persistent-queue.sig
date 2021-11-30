
signature PERSISTENT_QUEUE = sig

    type 'a queue

    (* I. Functions also found in PERSISTENT_ARRAY *)

    val maxLen : int
    val fromList : 'a list -> 'a queue 
    val tabulate : int * (int -> 'a) -> 'a queue
    val length : 'a queue -> int
    val sub : 'a queue * int -> 'a
    val vector : 'a queue -> 'a Vector.vector
    val app : ('a -> unit) -> 'a queue -> unit
    val appi : (int * 'a -> unit) -> 'a queue -> unit
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a queue -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a queue -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a queue -> 'b
    val update : 'a queue * int * 'a -> 'a queue
    val toList : 'a queue -> 'a list
    val map : ('a -> 'b) -> 'a queue -> 'b queue
    val mapi : (int * 'a -> 'b) -> 'a queue -> 'b queue
    val empty : 'a queue
    val isEmpty : 'a queue -> bool
    val append : 'a queue * 'a -> 'a queue
    val popEnd : 'a queue -> 'a queue * 'a
            
    (* II. Functions specific to PERSISTENT_QUEUE *)
            
    val queue : int * 'a -> 'a queue
    val prepend : 'a queue * 'a -> 'a queue
    val popStart : 'a queue -> 'a queue * 'a

end
