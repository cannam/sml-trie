
signature TRIE_ELEMENT = sig
    type t
    val compare : t * t -> order
    val toString : t -> string
end

signature TRIE = sig

    structure Elt : TRIE_ELEMENT

    type t

    type entry = Elt.t list

    val empty : t
    val add : t * entry -> t
    val enumerate : t -> entry list
    
end

functor TrieFn (E : TRIE_ELEMENT) : TRIE where type Elt.t = E.t = struct

    structure Elt = E

    type elt = Elt.t
    type entry = elt list
    
    structure Map = RedBlackMapFn (struct
                                    type ord_key = Elt.t
                                    val compare = Elt.compare
                                    end)

    datatype value = VALUE
                   | NO_VALUE

    datatype node = LEAF of value
                  | NODE of value * node Map.map

    type t = node

    val empty = LEAF NO_VALUE

    (*!!! this implementation assumes entries are unique -- adding one that already exists does nothing. we could alternatively count them. *)

    fun add (LEAF _, []) = LEAF VALUE
      | add (NODE (_, m), []) = NODE (VALUE, m)
      | add (LEAF v, x::xs) = NODE (v, Map.insert (Map.empty, x, add (empty, xs)))
      | add (NODE (v, m), x::xs) =
        case Map.find (m, x) of
            SOME n => NODE (v, Map.insert (m, x, add (n, xs)))
          | NONE => NODE (v, Map.insert (m, x, add (empty, xs)))

    fun concatMap f xx = List.concat (List.map f xx)

    fun string_of_entry e =
        String.concatWith " " (map Elt.toString e)

    fun enumerate trie =
        let fun enumerate' (acc, LEAF VALUE) = [rev acc]
              | enumerate' (acc, LEAF NO_VALUE) = []
              | enumerate' (acc, NODE (v, m)) =
                let val sub = concatMap (fn (e, n) => enumerate' (e :: acc, n))
                                        (Map.listItemsi m)
                in
                    if v = VALUE then (rev acc) :: sub else sub
                end
        in
            enumerate' ([], trie)
        end
            
(*			 
    fun match (trie, []) = enumerate trie
      | match (LEAF  = 

    fun match (trie, []) = enumerate trie
      | match (LEAF, _) = []
      | match (NODE n, first::rest) =
	case Map.find (n, first) of
	    SOME s => first :: (match (s, rest))
	  | NONE => []
*)			 
end

structure StringTrie = struct

    structure CharListTrie = TrieFn(struct
				     type t = char
				     val compare = Char.compare
				     val toString = Char.toString
				     end)

    type t = CharListTrie.t

    val empty = CharListTrie.empty

    fun add (trie, s) =
      CharListTrie.add (trie, String.explode s)

    fun enumerate trie =
      List.map String.implode (CharListTrie.enumerate trie)

end

structure StringTrieTest = struct

    fun test () =
        let
            val strings =
                ["poot", "parp", "par", "alligator", "zebra", "alliance", "aardvark"]
            val t = List.foldl (fn (s, t) => StringTrie.add (t, s))
                               StringTrie.empty
                               strings
	    val contents = StringTrie.enumerate t
        in
	    print ("contents: (" ^ (String.concatWith "," contents) ^ ")\n")
        end
      
end
