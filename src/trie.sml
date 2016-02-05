
signature TRIE = sig

    type t
    type entry

    val empty : t
    val add : t * entry -> t
    val contains : t * entry -> bool
    val remove : t * entry -> t
(*    val foldl : (entry * 'a -> 'a) -> 'a -> t -> 'a *)
    val enumerate : t -> entry list
    val match : t * entry -> entry list
(*    val foldl_match : (entry * 'a -> 'a) -> 'a -> (t * entry) -> 'a  *)
    val prefix_of : t * entry -> entry 
    
end

signature TRIE_ELEMENT = sig
    type t
    val compare : t * t -> order
    val toString : t -> string
end

functor TrieFn (E : TRIE_ELEMENT) :> TRIE where type entry = E.t list = struct

    type elt = E.t
    type entry = elt list
    
    structure Map = RedBlackMapFn (struct
                                    type ord_key = E.t
                                    val compare = E.compare
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

    fun remove (LEAF v, []) = LEAF NO_VALUE
      | remove (NODE (_, m), []) = NODE (NO_VALUE, m)
      | remove (LEAF v, x::xs) = LEAF v
      | remove (NODE (v, m), x::xs) =
        case Map.find (m, x) of
            SOME n => NODE (v, Map.insert (m, x, remove (n, xs)))
          | NONE => NODE (v, m)

    fun contains (LEAF VALUE, []) = true
      | contains (LEAF _, _) = false
      | contains (NODE (v, m), []) = v = VALUE
      | contains (NODE (v, m), x::xs) = 
        case Map.find (m, x) of
            SOME sub => contains (sub, xs)
          | NONE => false

    fun concatMap f xx = List.concat (List.map f xx)

    fun string_of_entry e =
        String.concatWith " " (map E.toString e)

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

    fun match (trie, []) = enumerate trie
      | match (LEAF _, x::xs) = []
      | match (NODE (v, m), x::xs) = 
        case Map.find (m, x) of
            SOME sub => map (fn entry => x :: entry) (match (sub, xs))
          | NONE => []

    fun prefix_of (trie, e) =
        let fun prefix' (best, acc, LEAF v, _) = if v = VALUE then acc else best
              | prefix' (best, acc, NODE (v, m), []) = if v = VALUE then acc else best
              | prefix' (best, acc, NODE (v, m), x::xs) =
                case Map.find (m, x) of
                    SOME sub => prefix' (if v = VALUE then acc else best,
                                         x :: acc,
                                         sub,
                                         xs)
                  | NONE => if v = VALUE then acc else best
        in
            rev (prefix' ([], [], trie, e))
        end

end

structure StringTrie :> TRIE where type entry = string = struct

    structure CharListTrie = TrieFn(struct
				     type t = char
				     val compare = Char.compare
				     val toString = Char.toString
				     end)

    type t = CharListTrie.t
    type entry = string

    val empty = CharListTrie.empty

    fun add (trie, s) =
        CharListTrie.add (trie, String.explode s)

    fun contains (trie, s) =
        CharListTrie.contains (trie, String.explode s)
                         
    fun remove (trie, s) =
        CharListTrie.remove (trie, String.explode s)

    fun enumerate trie =
        List.map String.implode (CharListTrie.enumerate trie)

    fun match (trie, s) =
        List.map String.implode (CharListTrie.match (trie, String.explode s))

    fun prefix_of (trie, s) =
        String.implode (CharListTrie.prefix_of (trie, (String.explode s)))
                 
end

structure StringTrieTest = struct

    fun test () =
        let
            val strings =
                ["poot", "parp", "par", "alligator", "zebra", "alliance", "aardvark","a"]
            val t = List.foldl (fn (s, t) => StringTrie.add (t, s))
                               StringTrie.empty
                               strings
            val t = StringTrie.remove (t, "poot")
	    val contents = StringTrie.enumerate t
            val match = StringTrie.match (t, "all")
        in
	    print ("contents: (" ^ (String.concatWith "," contents) ^ ")\n");
	    print ("match: (" ^ (String.concatWith "," match) ^ ")\n");
            print ("contains pa: " ^ (Bool.toString (StringTrie.contains (t, "pa"))) ^ "\n");
            print ("contains par: " ^ (Bool.toString (StringTrie.contains (t, "par"))) ^ "\n");
            print ("contains parp: " ^ (Bool.toString (StringTrie.contains (t, "parp"))) ^ "\n");
            print ("contains part: " ^ (Bool.toString (StringTrie.contains (t, "part"))) ^ "\n");
            print ("prefix_of pa: " ^ (StringTrie.prefix_of (t, "pa")) ^ "\n");
            print ("prefix_of par: " ^ (StringTrie.prefix_of (t, "par")) ^ "\n");
            print ("prefix_of parp: " ^ (StringTrie.prefix_of (t, "parp")) ^ "\n");
            print ("prefix_of part: " ^ (StringTrie.prefix_of (t, "part")) ^ "\n");
            print ("prefix_of \"\": " ^ (StringTrie.prefix_of (t, "")) ^ "\n");
            print ("prefix_of allia: " ^ (StringTrie.prefix_of (t, "allia")) ^ "\n")
        end
      
end
