
signature MTRIE_ELEMENT = sig
    type t
    val compare : t * t -> order
end

(* Turn a comparable list element type into a trie holding lists of
   that element type. Each level of the trie uses a map to hold
   pointers to its sub-nodes. This can be inefficient for deeper tries
   whose element type has few values, but may be suitable for wide
   flat structures. *)
                             
functor ListMTrieMapFn (E : MTRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = E.t where type key = E.t list = struct

    type element = E.t
    type key = element list
    type pattern = element option list
    
    structure Map = RedBlackMapFn (struct
                                    type ord_key = E.t
                                    val compare = E.compare
                                    end)

    datatype 'a item = VALUE of 'a
                     | NO_VALUE

    datatype 'a node = LEAF of 'a item
                     | NODE of 'a item * 'a node Map.map

    type 'a trie = 'a node

    val empty = LEAF NO_VALUE

    fun isEmpty (LEAF NO_VALUE) = true
      | isEmpty (LEAF _) = false
      | isEmpty (NODE (VALUE _, _)) = false
      | isEmpty (NODE (NO_VALUE, m)) = Map.all isEmpty m
            
    (*!!! check behaviour for replacing a value - I'm not sure the red-black map does what we want *)
    fun insert (NODE (i, m), x::xs, v) =
        (case Map.find (m, x) of
             SOME n => NODE (i, Map.insert (m, x, insert (n, xs, v)))
           | NONE => NODE (i, Map.insert (m, x, insert (empty, xs, v))))
      | insert (NODE (_, m), [], v) = NODE (VALUE v, m)
      | insert (LEAF _, [], v) = LEAF (VALUE v)
      | insert (LEAF i, x::xs, v) =
        NODE (i, Map.insert (Map.empty, x, insert (empty, xs, v)))

    fun remove (NODE (i, m), x::xs) =
        (case Map.find (m, x) of
             SOME n => NODE (i, Map.insert (m, x, remove (n, xs)))
           | NONE => NODE (i, m))
      | remove (NODE (_, m), []) = NODE (NO_VALUE, m)
      | remove (LEAF _, []) = LEAF NO_VALUE
      | remove (LEAF i, x::xs) = LEAF i

    fun find (NODE (_, m), x::xs) = 
        (case Map.find (m, x) of
            SOME sub => find (sub, xs)
          | NONE => NONE)
      | find (NODE (VALUE v, _), []) = SOME v
      | find (LEAF (VALUE v), []) = SOME v
      | find _ = NONE

    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false

    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NODE (i, m)) =
        List.foldl (fn ((e, LEAF (VALUE v)), acc) => f (rev (e :: rpfx), v, acc)
                     | ((e, n), acc) => foldli_helper f (acc, e :: rpfx, n))
                   (case i of
                        VALUE v => f (rev rpfx, v, acc)
                      | NO_VALUE => acc)
                   (Map.listItemsi m)                      
      | foldli_helper f (acc, rpfx, LEAF (VALUE v)) = f (rev rpfx, v, acc)
      | foldli_helper f (acc, rpfx, LEAF NO_VALUE) = acc

    fun foldl f acc trie =
        foldli_helper (fn (k, v, acc) => f (v, acc)) (acc, [], trie)
                      
    fun foldli f acc trie =
        foldli_helper f (acc, [], trie)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch f acc (trie, e) =
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, NODE (_, m), x::xs) =
                (case Map.find (m, x) of
                     SOME sub => fold' (acc, x :: rpfx, sub, xs)
                   | NONE => acc)
              | fold' (acc, rpfx, trie, []) = foldli_helper f (acc, rpfx, trie)
              | fold' (acc, rpfx, LEAF _, _) = acc
        in
            fold' (acc, [], trie, e)
        end

    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch f acc (trie, p) =
        let fun fold' (acc, pfx, NODE (_, m), (SOME x)::xs) =
                (case Map.find (m, x) of
                     SOME sub => fold' (acc, x :: pfx, sub, xs)
                   | NONE => acc)
              | fold' (acc, pfx, LEAF (VALUE v), []) = f (rev pfx, v, acc)
	      | fold' (acc, pfx, NODE (VALUE v, _), []) = f (rev pfx, v, acc)
	      | fold' (acc, pfx, _, []) = acc
              | fold' (acc, pfx, LEAF _, _) = acc
              | fold' (acc, pfx, NODE (_, m), NONE::xs) =
                List.foldl (fn ((e, n), acc) => fold' (acc, e :: pfx, n, xs))
                           acc
                           (Map.listItemsi m)
        in
            fold' (acc, [], trie, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (trie, e) =
        let fun prefix' (best, acc, NODE (i, m), x::xs) =
                let val best =
                        case i of
                            VALUE _ => acc
                          | NO_VALUE => best
                in
                    case Map.find (m, x) of
                        SOME sub => prefix' (best, x :: acc, sub, xs)
                      | NONE => best
                end
              | prefix' (best, acc, LEAF (VALUE _), _) = acc
              | prefix' (best, acc, NODE (VALUE _, _), []) = acc
              | prefix' (best, acc, LEAF (NO_VALUE), _) = best
              | prefix' (best, acc, NODE (NO_VALUE, _), []) = best
        in
	    rev (prefix' ([], [], trie, e))
        end

end
                                                                        
