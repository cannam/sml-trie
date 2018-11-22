
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature LIST_TRIE_NODE_MAP = sig
    type key
    type 'a map
    val new : unit -> 'a map
    val isEmpty : 'a map -> bool
    val find : 'a map * key -> 'a option
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
                               
    datatype 'a node = NO_NODE
                     | NODE of 'a option * 'a node M.map

    type 'a trie = 'a node
                      
    val empty = NO_NODE
                                                         
    fun isEmpty NO_NODE = true
      | isEmpty _ = false

    fun update (n, xx, f) =
        case (n, xx) of
            (NO_NODE, []) => NODE (SOME (f NONE), M.new ())
          | (NO_NODE, x::xs) => NODE (NONE, M.update (M.new (), x,
                                                      update (NO_NODE, xs, f)))
          | (NODE (item, vec), []) => NODE (SOME (f item), vec)
          | (NODE (item, vec), x::xs) => 
            case M.find (vec, x) of
                NONE =>
                NODE (item, M.update (vec, x, update (NO_NODE, xs, f)))
              | SOME nsub =>
                NODE (item, M.update (vec, x, update (nsub, xs, f)))

    fun insert (n, xx, v) =
        update (n, xx, fn _ => v)

    fun remove (n, xx) =
        case (n, xx) of
            (NO_NODE, _) => NO_NODE
          | (NODE (item, vec), []) => if M.isEmpty vec
                                      then NO_NODE
                                      else NODE (NONE, vec)
          | (n as NODE (item, vec), x::xs) =>
            case M.find (vec, x) of
                NONE => n
              | SOME nsub =>
                case remove (nsub, xs) of
                    NODE rn => NODE (item, M.update (vec, x, NODE rn))
                  | NO_NODE =>
                    let val vv = M.remove (vec, x)
                    in
                        case item of
                            SOME _ => NODE (item, vv)
                          | NONE => if M.isEmpty vv
                                    then NO_NODE
                                    else NODE (item, vv)
                    end

    fun find (NO_NODE, _) = NONE
      | find (NODE (item, _), []) = item
      | find (NODE (item, vec), x::xs) =
        case M.find (vec, x) of
            NONE => NONE
          | SOME nsub => find (nsub, xs)

    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
                     
    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NO_NODE) = acc
      | foldli_helper f (acc, rpfx, NODE (item, vec)) =
        M.foldli (fn (x, NO_NODE, acc) => acc
                   | (x, n, acc) => foldli_helper f (acc, x :: rpfx, n))
                 (case item of
                      NONE => acc
                    | SOME v => f (rev rpfx, v, acc))
                 vec

    fun foldl f acc NO_NODE = acc
      | foldl f acc n = 
        foldli_helper (fn (k, v, acc) => f (v, acc)) (acc, [], n)
                      
    fun foldli f acc NO_NODE = acc
      | foldli f acc n = 
        foldli_helper f (acc, [], n)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch f acc (NO_NODE, e) = acc
      | foldliPrefixMatch f acc (node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, n, []) = foldli_helper f (acc, rpfx, n)
              | fold' (acc, rpfx, NO_NODE, x::xs) = acc
              | fold' (acc, rpfx, NODE (item, vec), x::xs) =
                case M.find (vec, x) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: rpfx, nsub, xs)
        in
            fold' (acc, [], node, e)
        end

    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch f acc (NO_NODE, p) = acc
      | foldliPatternMatch f acc (node, p) =
        let fun fold' (acc, pfx, NO_NODE, _) = acc
              | fold' (acc, pfx, NODE (NONE, _), []) = acc
              | fold' (acc, pfx, NODE (SOME v, _), []) = f (rev pfx, v, acc)
              | fold' (acc, pfx, NODE (_, vec), NONE::xs) =
                M.foldli (fn (x, NO_NODE, acc) => acc
                           | (x, n, acc) => fold' (acc, x :: pfx, n, xs))
                         acc vec
              | fold' (acc, pfx, NODE (_, vec), (SOME x)::xs) =
                case M.find (vec, x) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: pfx, nsub, xs)
        in
            fold' (acc, [], node, p)
        end
    
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (NO_NODE, e) = []
      | prefixOf (node, e) = 
        let fun prefix' (best, acc, NO_NODE, _) = best
              | prefix' (best, acc, n as NODE (item, vec), x::xs) =
                let val best = case item of
                                   NONE => best
                                 | SOME _ => acc
                in
                    prefix' (best,
                             x :: acc,
                             case M.find (vec, x) of
                                 NONE => NO_NODE
                               | SOME nsub => nsub,
                             xs)
                end
              | prefix' (best, acc, NODE (SOME _, _), []) = acc
              | prefix' (best, acc, NODE (NONE, _), []) = best
        in
	    rev (prefix' ([], [], node, e))
        end

end

