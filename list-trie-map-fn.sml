
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature LIST_TRIE_NODE_MAP = sig
    type key
    type 'a map
    val new : unit -> 'a map
    val isEmpty : 'a map -> bool
    val find : 'a map * key -> 'a option
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val update : 'a map * key * 'a -> 'a map
    val remove : 'a map * key -> 'a map
end
                                          
functor ListTrieMapFn (M : LIST_TRIE_NODE_MAP)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = M.key where type key = M.key list = struct

    type element = M.key
    type key = element list
    type pattern = element option list

    datatype 'a node = LEAF of 'a
                     | NODE of 'a option * 'a node M.map

    type 'a trie = 'a node

    fun new () = NODE (NONE, M.new ())

    fun isEmpty (NODE (NONE, m)) = M.isEmpty m
      | isEmpty _ = false

    fun update (n, xx, f) =
        case (n, xx) of
            (LEAF item, []) => LEAF (f (SOME item))
          | (LEAF item, x::xs) => update (NODE (SOME item, M.new ()), x::xs, f)
          | (NODE (item, map), []) => NODE (SOME (f item), map)
          | (NODE (item, map), [x]) =>
            (case M.find (map, x) of
                 NONE =>
                 NODE (item, M.update (map, x, LEAF (f NONE)))
               | SOME nsub =>
                 NODE (item, M.update (map, x, update (nsub, [], f))))
          | (NODE (item, map), x::xs) => 
            (case M.find (map, x) of
                 NONE =>
                 NODE (item, M.update (map, x,
                                       update (NODE (NONE, M.new ()), xs, f)))
               | SOME nsub =>
                 NODE (item, M.update (map, x, update (nsub, xs, f))))

    fun insert (n, xx, v) =
        update (n, xx, fn _ => v)

    fun remove (n, xx) =
        case (n, xx) of
            (LEAF _, []) => NODE (NONE, M.new ())
          | (LEAF item, _) => LEAF item
          | (NODE (item, map), []) => NODE (NONE, map)
          | (n as NODE (item, map), x::xs) =>
            case M.find (map, x) of
                NONE => n
              | SOME nsub =>
                let val nsub' = remove (nsub, xs)
                in
                    if isEmpty nsub'
                    then let val map' = M.remove (map, x)
                         in
                             if M.isEmpty map'
                             then case item of
                                      NONE => NODE (item, map')
                                    | SOME item => LEAF item
                             else NODE (item, map')
                         end
                    else NODE (item, M.update (map, x, nsub'))
                end

    fun find (LEAF item, []) = SOME item
      | find (LEAF _, _) = NONE
      | find (NODE (item, _), []) = item
      | find (NODE (item, map), x::xs) =
        case M.find (map, x) of
            NONE => NONE
          | SOME nsub => find (nsub, xs)

    fun lookup (t, k) =
        case find (t, k) of
            NONE => raise Subscript
          | SOME v => v
                              
    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false

    fun foldl f acc n =
        let fun fold' (acc, LEAF v) = f (v, acc)
              | fold' (acc, NODE (item, map)) =
                M.foldl (fn (n, acc) => fold' (acc, n))
                        (case item of
                             NONE => acc
                           | SOME v => f (v, acc))
                        map
        in
            fold' (acc, n)
        end
                     
    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, LEAF v) = f (rev rpfx, v, acc)
      | foldli_helper f (acc, rpfx, NODE (item, map)) =
        M.foldli (fn (x, n, acc) => foldli_helper f (acc, x :: rpfx, n))
                 (case item of
                      NONE => acc
                    | SOME v => f (rev rpfx, v, acc))
                 map
                      
    fun foldli f acc n = 
        foldli_helper f (acc, [], n)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch f acc (node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, n, []) = foldli_helper f (acc, rpfx, n)
              | fold' (acc, rpfx, LEAF _, x::xs) = acc
              | fold' (acc, rpfx, NODE (item, map), x::xs) =
                case M.find (map, x) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: rpfx, nsub, xs)
        in
            fold' (acc, [], node, e)
        end

    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch f acc (node, p) =
        let fun fold' (acc, rpfx, LEAF v, []) = f (rev rpfx, v, acc)
              | fold' (acc, rpfx, LEAF _, _) = acc
              | fold' (acc, rpfx, NODE (NONE, _), []) = acc
              | fold' (acc, rpfx, NODE (SOME v, _), []) = f (rev rpfx, v, acc)
              | fold' (acc, rpfx, NODE (_, map), NONE::xs) =
                M.foldli (fn (x, n, acc) => fold' (acc, x :: rpfx, n, xs))
                         acc map
              | fold' (acc, rpfx, NODE (_, map), (SOME x)::xs) =
                case M.find (map, x) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: rpfx, nsub, xs)
        in
            fold' (acc, [], node, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (node, e) = 
        let fun prefix' (best, acc, n as NODE (item, map), x::xs) =
                let val best = case item of
                                   NONE => best
                                 | SOME _ => acc
                in
                    prefix' (best,
                             x :: acc,
                             case M.find (map, x) of
                                 NONE => NODE (NONE, M.new ())
                               | SOME nsub => nsub,
                             xs)
                end
              | prefix' (best, acc, NODE (SOME _, _), []) = acc
              | prefix' (best, acc, NODE (NONE, _), []) = best
              | prefix' (best, acc, LEAF _, _) = acc
        in
	    rev (prefix' ([], [], node, e))
        end

end

