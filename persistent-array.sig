
signature PERSISTENT_ARRAY = sig

    type 'a array (* nb not an eqtype *)

    val maxLen : int

    val fromList : 'a list -> 'a array 
    val tabulate : int * (int -> 'a) -> 'a array
    val length : 'a array -> int

    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> 'a array

    val appi : (int * 'a -> unit) -> 'a array -> unit
    val app : ('a -> unit) -> 'a array -> unit

    val mapi : (int * 'a -> 'b) -> 'a array -> 'b array
    val map : ('a -> 'b) -> 'a array -> 'b array

    val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b

    val empty : 'a array
    val isEmpty : 'a array -> bool

    val append : 'a array * 'a -> 'a array
    val popEnd : 'a array -> 'a array * 'a

    val toList : 'a array -> 'a list

    (* !!! + collate etc *)
end
