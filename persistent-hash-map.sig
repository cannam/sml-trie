
(* Copyright 2015-2018 Chris Cannam.
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

    (** Insert a key-value pair, returning a new hash map. If the key
        is already present, its value will be updated in the new map *)
    val insert : 'a hash_map * hash_key * 'a -> 'a hash_map

    (** Return the hash map with the given key removed. If the key is
        not present, the returned hash map will be unchanged *)
    val remove : 'a hash_map * hash_key -> 'a hash_map
                                          
    (** Test whether the hash map contains the given key *)
    val contains : 'a hash_map * hash_key -> bool

    (** Look for a key and return its corresponding value, or NONE if
        the key is not present in the hash map *)
    val find : 'a hash_map * hash_key -> 'a option

    (** Look for a key and return its corresponding value, raising
        Subscript if the key is not present in the hash map *)
    val lookup : 'a hash_map * hash_key -> 'a
                                            
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

(*!!! + tabulate, union/intersection etc *)
                                                   
end                   
