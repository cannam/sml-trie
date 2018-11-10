
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature ARRAY_TRIE_ELEMENT = sig
    type t
    val ord : t -> int
    val invOrd : int -> t
    val maxOrd : int
end

(* Turn a type that can be compactly converted into an integer
   ordering into a trie holding lists of that type. Each level of the
   trie uses a vector to hold pointers to its sub-nodes, indexed by
   integer value of the type. Because this is an immutable structure
   and updating a vector can be slow, this may be inefficient for
   "wide" tries with many sub-nodes per node and a lot of inserts. It
   may be more efficient for deep tries, e.g. of character lists, with
   lots of lookups. *)
                             
functor ListArrayTrieFn (E : ARRAY_TRIE_ELEMENT)
	:> PATTERN_MATCH_TRIE
	       where type element = E.t where type entry = E.t list = struct

    type element = E.t
    type entry = element list
    type pattern = element option list

    datatype value = VALUE
                   | NO_VALUE

    datatype node = LEAF of value
                  | NODE of value * node vector

    type t = node
    type trie = t

    val empty = LEAF NO_VALUE

    fun add (LEAF _, []) = LEAF VALUE
      | add (NODE (_, m), []) = NODE (VALUE, m)
      | add (n, x::xs) =
        let val ix = E.ord x
        in
            case n of
                LEAF v => 
                NODE (v, Vector.tabulate (E.maxOrd, fn i => if i = ix
                                                            then add (empty, xs)
                                                            else empty))
              | NODE (v, nn) => 
                NODE (v, Vector.update (nn, ix, add (Vector.sub (nn, ix), xs)))
        end

    fun remove (LEAF _, []) = LEAF NO_VALUE
      | remove (NODE (_, nn), []) = NODE (NO_VALUE, nn)
      | remove (LEAF v, x::xs) = LEAF v
      | remove (NODE (v, nn), x::xs) =
        let val ix = E.ord x
        in
            NODE (v, Vector.update (nn, ix, remove (Vector.sub (nn, ix), xs)))
        end

    fun contains (LEAF VALUE, []) = true
      | contains (LEAF _, _) = false
      | contains (NODE (v, _), []) = (v = VALUE)
      | contains (NODE (v, nn), x::xs) =
        let val ix = E.ord x
        in
            contains (Vector.sub (nn, ix), xs)
        end

    fun concatMap f xx = List.concat (List.map f xx)

    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldl_helper f (acc, rpfx, NODE (v, nn)) =
        Vector.foldli (fn (ix, LEAF VALUE, acc) => f (rev (E.invOrd ix :: rpfx), acc)
                        | (_, LEAF NO_VALUE, acc) => acc (* ??? *)
                        | (ix, n, acc) => foldl_helper f (acc, E.invOrd ix :: rpfx, n))
                     (if v = VALUE then f (rev rpfx, acc) else acc)
                     nn
      | foldl_helper f (acc, rpfx, LEAF VALUE) = f (rev rpfx, acc)
      | foldl_helper f (acc, rpfx, LEAF NO_VALUE) = acc

    fun foldl f acc trie = foldl_helper f (acc, [], trie)

    fun enumerate trie = rev (foldl (op::) [] trie)

    fun foldlPrefixMatch f acc (trie, e) =
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, NODE (v, nn), x::xs) =
                fold' (acc, x :: rpfx, Vector.sub (nn, E.ord x), xs)
              | fold' (acc, rpfx, trie, []) = foldl_helper f (acc, rpfx, trie)
              | fold' (acc, rpfx, LEAF _, _) = acc
        in
            fold' (acc, [], trie, e)
        end

    fun prefixMatch (trie, e) = rev (foldlPrefixMatch (op::) [] (trie, e))

    fun foldlPatternMatch f acc (trie, p) =
        let fun fold' (acc, pfx, NODE (v, nn), (SOME x)::xs) =
                fold' (acc, x :: pfx, Vector.sub (nn, E.ord x), xs)
              | fold' (acc, pfx, LEAF VALUE, []) = f (rev pfx, acc)
	      | fold' (acc, pfx, NODE (VALUE, _), []) = f (rev pfx, acc)
	      | fold' (acc, pfx, _, []) = acc
              | fold' (acc, pfx, LEAF _, _) = acc
              | fold' (acc, pfx, NODE (v, nn), NONE::xs) =
                Vector.foldli (fn (ix, n, acc) =>
                                  fold' (acc, E.invOrd ix :: pfx, n, xs))
                              acc
                              nn
        in
            fold' (acc, [], trie, p)
        end
    
    fun patternMatch (trie, p) = rev (foldlPatternMatch (op::) [] (trie, p))

    fun prefixOf (trie, e) =
        let fun prefix' (best, acc, NODE (v, nn), x::xs) =
                let val best = if v = VALUE then acc else best
                in
                    prefix' (best, x :: acc, Vector.sub (nn, E.ord x), xs)
                end
              | prefix' (best, acc, LEAF VALUE, _) = acc
              | prefix' (best, acc, NODE (VALUE, _), []) = acc
              | prefix' (best, acc, LEAF NO_VALUE, _) = best
              | prefix' (best, acc, NODE (NO_VALUE, _), []) = best
        in
	    rev (prefix' ([], [], trie, e))
        end

end

