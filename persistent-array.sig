
signature PERSISTENT_ARRAY = sig

    type 'a array (* nb not an eqtype *)

    (* I. Functions identical to ARRAY *)
            
    val maxLen : int
    val array : int * 'a -> 'a array
    val fromList : 'a list -> 'a array 
    val tabulate : int * (int -> 'a) -> 'a array
    val length : 'a array -> int
    val sub : 'a array * int -> 'a
    val vector : 'a array -> 'a Vector.vector
    val app : ('a -> unit) -> 'a array -> unit
    val appi : (int * 'a -> unit) -> 'a array -> unit
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val find : ('a -> bool) -> 'a array -> 'a option
    val findi : (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val exists : ('a -> bool) -> 'a array -> bool
    val all : ('a -> bool) -> 'a array -> bool
    val collate : ('a * 'a -> order) -> 'a array * 'a array -> order

    (* II. Functions similar to ARRAY, but altered for persistent
           array. The functions passed to modify/modifyi should return
           NONE for "no change" or SOME x to change the value. *)

    val update : 'a array * int * 'a -> 'a array
    val modify : ('a -> 'a option) -> 'a array -> 'a array
    val modifyi : (int * 'a -> 'a option) -> 'a array -> 'a array

    (* III. Functions not in ARRAY *)
                                                     
    val toList : 'a array -> 'a list
    val map : ('a -> 'b) -> 'a array -> 'b array
    val mapi : (int * 'a -> 'b) -> 'a array -> 'b array
    val empty : 'a array
    val isEmpty : 'a array -> bool
    val append : 'a array * 'a -> 'a array
    val popEnd : 'a array -> 'a array * 'a
                                
end
