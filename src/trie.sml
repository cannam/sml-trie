
signature TRIE_ELEMENT = sig
    type t
    val compare : t * t -> order
end

functor ListEntryTrieFn (E : TRIE_ELEMENT) :> LIST_ENTRY_TRIE where type element = E.t where type entry = E.t list = struct

    type element = E.t
    type entry = element list
    type pattern = element option list
    
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

    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldl_helper f (acc, rpfx, LEAF VALUE) = f (rev rpfx, acc)
      | foldl_helper f (acc, rpfx, LEAF NO_VALUE) = acc
      | foldl_helper f (acc, rpfx, NODE (v, m)) =
        List.foldl (fn ((e, n), acc) => foldl_helper f (acc, e :: rpfx, n))
                   (if v = VALUE then f (rev rpfx, acc) else acc)
                   (Map.listItemsi m)                      
                          
    fun foldl f acc trie = foldl_helper f (acc, [], trie)

    fun enumerate trie = rev (foldl (op::) [] trie)

    fun foldl_prefix_match f acc (trie, e) =
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun foldl_match' (acc, rpfx, trie, []) =
                foldl_helper f (acc, rpfx, trie)
              | foldl_match' (acc, rpfx, LEAF _, _) = acc
              | foldl_match' (acc, rpfx, NODE (v, m), x::xs) =
                case Map.find (m, x) of
                    SOME sub => foldl_match' (acc, x :: rpfx, sub, xs)
                  | NONE => acc
        in
            foldl_match' (acc, [], trie, e)
        end

    fun prefix_match (trie, e) = rev (foldl_prefix_match (op::) [] (trie, e))

    fun foldl_pattern_match f acc (trie, p) =
        let fun foldl_match' (acc, pfx, trie, []) = f (rev pfx, acc)
              | foldl_match' (acc, pfx, LEAF _, _) = acc
              | foldl_match' (acc, pfx, NODE (v, m), NONE::xs) =
                List.foldl (fn ((e, n), acc) => foldl_match' (acc, e :: pfx, n, xs))
                           acc
                           (Map.listItemsi m)
              | foldl_match' (acc, pfx, NODE (v, m), (SOME x)::xs) =
                case Map.find (m, x) of
                    SOME sub => foldl_match' (acc, x :: pfx, sub, xs)
                  | NONE => acc
        in
            foldl_match' (acc, [], trie, p)
        end
    
    fun prefix_of (trie, e) =
        let fun prefix' (best, acc, LEAF VALUE, _) = acc
              | prefix' (best, acc, NODE (VALUE, _), []) = acc
              | prefix' (best, acc, LEAF NO_VALUE, _) = best
              | prefix' (best, acc, NODE (NO_VALUE, _), []) = best
              | prefix' (best, acc, NODE (v, m), x::xs) =
                let val best = if v = VALUE then acc else best
                in
                    case Map.find (m, x) of
                        SOME sub => prefix' (best, x :: acc, sub, xs)
                      | NONE => best
                end
        in
            rev (prefix' ([], [], trie, e))
        end

end

structure StringTrie :> TRIE where type entry = string = struct

    structure CharListTrie = ListEntryTrieFn(struct
				              type t = char
				              val compare = Char.compare
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

    fun foldl f acc trie =
        CharListTrie.foldl (fn (e, acc) => f (String.implode e, acc))
                           acc trie
                 
    fun prefix_match (trie, s) =
        List.map String.implode (CharListTrie.prefix_match (trie, String.explode s))

    fun foldl_prefix_match f acc (trie, s) =
        CharListTrie.foldl_prefix_match (fn (e, acc) => f (String.implode e, acc))
                                 acc (trie, String.explode s)

    fun prefix_of (trie, s) =
        String.implode (CharListTrie.prefix_of (trie, (String.explode s)))
                 
end
