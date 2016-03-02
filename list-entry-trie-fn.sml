
signature TRIE_ELEMENT = sig
    type t
    val compare : t * t -> order
end

(* Turn a comparable list element type into a trie containing lists of
   that element type. *)
                             
functor ListEntryTrieFn (E : TRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE
	       where type element = E.t where type entry = E.t list = struct

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
    type trie = t

    val empty = LEAF NO_VALUE

    fun add (NODE (v, m), x::xs) =
        (case Map.find (m, x) of
             SOME n => NODE (v, Map.insert (m, x, add (n, xs)))
           | NONE => NODE (v, Map.insert (m, x, add (empty, xs))))
      | add (NODE (_, m), []) = NODE (VALUE, m)
      | add (LEAF _, []) = LEAF VALUE
      | add (LEAF v, x::xs) = NODE (v, Map.insert (Map.empty, x, add (empty, xs)))

    fun remove (NODE (v, m), x::xs) =
        (case Map.find (m, x) of
             SOME n => NODE (v, Map.insert (m, x, remove (n, xs)))
           | NONE => NODE (v, m))
      | remove (NODE (_, m), []) = NODE (NO_VALUE, m)
      | remove (LEAF _, []) = LEAF NO_VALUE
      | remove (LEAF v, x::xs) = LEAF v

    fun contains (NODE (v, m), x::xs) = 
        (case Map.find (m, x) of
            SOME sub => contains (sub, xs)
          | NONE => false)
      | contains (NODE (v, m), []) = (v = VALUE)
      | contains (LEAF VALUE, []) = true
      | contains (LEAF _, _) = false

    fun concatMap f xx = List.concat (List.map f xx)

    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldl_helper f (acc, rpfx, NODE (v, m)) =
        List.foldl (fn ((e, LEAF VALUE), acc) => f (rev (e :: rpfx), acc)
                     | ((e, n), acc) => foldl_helper f (acc, e :: rpfx, n))
                   (if v = VALUE then f (rev rpfx, acc) else acc)
                   (Map.listItemsi m)                      
      | foldl_helper f (acc, rpfx, LEAF VALUE) = f (rev rpfx, acc)
      | foldl_helper f (acc, rpfx, LEAF NO_VALUE) = acc

    fun foldl f acc trie = foldl_helper f (acc, [], trie)

    fun enumerate trie = rev (foldl (op::) [] trie)

    fun foldl_prefix_match f acc (trie, e) =
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, NODE (v, m), x::xs) =
                (case Map.find (m, x) of
                     SOME sub => fold' (acc, x :: rpfx, sub, xs)
                   | NONE => acc)
              | fold' (acc, rpfx, trie, []) = foldl_helper f (acc, rpfx, trie)
              | fold' (acc, rpfx, LEAF _, _) = acc
        in
            fold' (acc, [], trie, e)
        end

    fun prefix_match (trie, e) = rev (foldl_prefix_match (op::) [] (trie, e))

    fun foldl_pattern_match f acc (trie, p) =
        let fun fold' (acc, pfx, NODE (v, m), (SOME x)::xs) =
                (case Map.find (m, x) of
                    SOME sub => fold' (acc, x :: pfx, sub, xs)
                  | NONE => acc)
              | fold' (acc, pfx, LEAF VALUE, []) = f (rev pfx, acc)
	      | fold' (acc, pfx, NODE (VALUE, _), []) = f (rev pfx, acc)
	      | fold' (acc, pfx, _, []) = acc
              | fold' (acc, pfx, LEAF _, _) = acc
              | fold' (acc, pfx, NODE (v, m), NONE::xs) =
                List.foldl (fn ((e, n), acc) => fold' (acc, e :: pfx, n, xs))
                           acc
                           (Map.listItemsi m)
        in
            fold' (acc, [], trie, p)
        end
    
    fun pattern_match (trie, p) = rev (foldl_pattern_match (op::) [] (trie, p))

    fun prefix_of (trie, e) =
        let fun prefix' (best, acc, NODE (v, m), x::xs) =
                let val best = if v = VALUE then acc else best
                in
                    case Map.find (m, x) of
                        SOME sub => prefix' (best, x :: acc, sub, xs)
                      | NONE => best
                end
              | prefix' (best, acc, LEAF VALUE, _) = acc
              | prefix' (best, acc, NODE (VALUE, _), []) = acc
              | prefix' (best, acc, LEAF NO_VALUE, _) = best
              | prefix' (best, acc, NODE (NO_VALUE, _), []) = best
        in
	    rev (prefix' ([], [], trie, e))
        end

end

