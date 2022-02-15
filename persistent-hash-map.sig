
(* Copyright 2015-2021 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature HASH_KEY = sig

    type hash_key

    (* Compute an unsigned integer key from a hash key *)
    val hashVal : hash_key -> Word32.word

    (* Return true if two keys are the same *)
    val sameKey : (hash_key * hash_key) -> bool

end

signature PERSISTENT_HASH_MAP = sig

    type 'a hash_map
    type hash_key

    (** Empty hash map *)
    val empty : 'a hash_map

    (** Test whether a hash map is empty *)
    val isEmpty : 'a hash_map -> bool

    (** Create a singleton map *)
    val singleton : hash_key * 'a -> 'a hash_map
                                     
    (** Insert a key-value pair, returning a new hash map. If the key
        is already present, its value will be updated in the new map *)
    val insert : 'a hash_map * hash_key * 'a -> 'a hash_map

    (** Look for a key and return its corresponding value, or NONE if
        the key is not present in the hash map *)
    val find : 'a hash_map * hash_key -> 'a option

    (** Look for a key and return its corresponding value, raising
        Subscript if the key is not present in the hash map *)
    val lookup : 'a hash_map * hash_key -> 'a
                                          
    (** Test whether the hash map contains the given key *)
    val contains : 'a hash_map * hash_key -> bool

    (** Return the hash map with the given key removed. If the key is
        not present, the returned hash map will be unchanged *)
    val remove : 'a hash_map * hash_key -> 'a hash_map

    (** Alter a key-value pair in the hash map, returning a new hash
        map. The function argument should map from the previous value
        associated with the key, or NONE if it was absent before, to
        the new value, or NONE if it is to be removed. (This is called
        alter rather than modify to avoid confusion with the array
        modify functions, which do something rather different) *)
    val alter : 'a hash_map * hash_key * ('a option -> 'a option) -> 'a hash_map

    (** Create a new hash map by applying the given map function to
        the values in this hash map *)
    val map : ('a -> 'b) -> 'a hash_map -> 'b hash_map

    (** Create a new hash map by applying the given map function to
        the key-value pairs in this hash map *)
    val mapi : (hash_key * 'a -> 'b) -> 'a hash_map -> 'b hash_map
                                      
    (** Fold over all the values in the hash map, in sort order *)
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a hash_map -> 'b

    (** Fold over all the key-value pairs in the hash map, in sort order *)
    val foldli : (hash_key * 'a * 'b -> 'b) -> 'b -> 'a hash_map -> 'b
                                            
    (** Fold over all the values in the hash map, in sort order *)
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a hash_map -> 'b

    (** Fold over all the key-value pairs in the hash map, in sort order *)
    val foldri : (hash_key * 'a * 'b -> 'b) -> 'b -> 'a hash_map -> 'b

    (** Return a list of all key-value pairs in the hash map, in sort order *)
    val enumerate : 'a hash_map -> (hash_key * 'a) list

    (** Return a list of all keys in the hash map, in sort order *)
    val listKeys : 'a hash_map -> hash_key list

    (** Return a hash map derived from this one, in which the elements
        that do not satisfy the given predicate have been removed *)
    val filter : ('a -> bool) -> 'a hash_map -> 'a hash_map

    (** Return a hash map derived from this one, in which the elements
        that do not satisfy the given predicate have been removed *)
    val filteri : (hash_key * 'a -> bool) -> 'a hash_map -> 'a hash_map

                                                               
(*!!! + tabulate, union/intersection etc *)
                                                   

    (** SML/NJ ORD_MAP compatibility name for "contains" *)
    val inDomain : 'a hash_map * hash_key -> bool

    (** SML/NJ ORD_MAP compatibility name for "enumerate" *)
    val listItemsi : 'a hash_map -> (hash_key * 'a) list
                                                 
end                   
