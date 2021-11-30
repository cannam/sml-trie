
signature PERSISTENT_ARRAY_SLICE = sig

    structure PersistentArray : PERSISTENT_ARRAY
                  
    type 'a slice 
        
    val length : 'a slice -> int
    val sub : 'a slice * int -> 'a
    val full : 'a PersistentArray.array -> 'a slice
    val slice : 'a PersistentArray.array * int * int option -> 'a slice
    val subslice : 'a slice * int * int option -> 'a slice
    val base : 'a slice -> 'a PersistentArray.array * int * int
    val array : 'a slice -> 'a PersistentArray.array
    val isEmpty : 'a slice -> bool
    val app  : ('a -> unit) -> 'a slice -> unit
    val appi : (int * 'a -> unit) -> 'a slice -> unit
    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
            
end
