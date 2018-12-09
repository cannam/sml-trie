
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
    val update : 'a map * key * ('a option -> 'a) -> 'a map
    val remove : 'a map * key -> 'a map
end
                                          
functor ListTrieMapFn (M : LIST_TRIE_NODE_MAP)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = M.key where type key = M.key list = struct

    type element = M.key
    type key = element list
    type pattern = element option list
                               
    datatype 'a node = NODE of 'a option * 'a node M.map

    datatype 'a trie = EMPTY
                     | POPULATED of 'a node

    fun newNode () = NODE (NONE, M.new ())
        
    val empty = EMPTY
                                                         
    fun isEmptyNode (NODE (NONE, m)) = M.isEmpty m
      | isEmptyNode _ = false

    fun isEmpty EMPTY = true
      | isEmpty _ = false

    fun update' (n, xx, f) =
        case (n, xx) of
            (NODE (item, vec), []) => NODE (SOME (f item), vec)
          | (NODE (item, vec), x::xs) =>
            NODE (item,
                  M.update (vec, x, fn NONE => update' (newNode (), xs, f)
                                     | SOME nsub => update' (nsub, xs, f)))

    fun update (EMPTY, xx, f) = POPULATED (update' (newNode(), xx, f))
      | update (POPULATED n, xx, f) = POPULATED (update' (n, xx, f))

    fun insert (n, xx, v) =
        update (n, xx, fn _ => v)

    fun remove' (n, xx) =
        case (n, xx) of
            (NODE (item, vec), []) => NODE (NONE, vec)
          | (n as NODE (item, vec), x::xs) =>
            case M.find (vec, x) of
                NONE => n
              | SOME nsub =>
                let val nsub' = remove' (nsub, xs)
                in
                    if isEmptyNode nsub'
                    then 
                        let val vv = M.remove (vec, x)
                        in
                            case item of
                                SOME _ => NODE (item, vv)
                              | NONE => NODE (item, vv)
                        end
                    else NODE (item, M.update (vec, x, fn _ => nsub'))
                end

    fun remove (EMPTY, _) = EMPTY
      | remove (POPULATED n, xx) =
        let val n' = remove' (n, xx)
        in
            if isEmptyNode n'
            then EMPTY
            else POPULATED n'
        end
                    
    fun find' (NODE (item, _), []) = item
      | find' (NODE (item, vec), x::xs) =
        case M.find (vec, x) of
            NONE => NONE
          | SOME nsub => find' (nsub, xs)

    fun find (EMPTY, _) = NONE
      | find (POPULATED n, xx) =
        find' (n, xx)
                               
    fun lookup (t, k) =
        case find (t, k) of
            NONE => raise Subscript
          | SOME v => v
                              
    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
             
    fun foldl f acc t =
        let fun fold' (acc, NODE (item, map)) =
                M.foldl (fn (n, acc) => fold' (acc, n))
                        (case item of
                             NONE => acc
                           | SOME v => f (v, acc))
                        map
        in
            case t of
                EMPTY => acc
              | POPULATED n => fold' (acc, n)
        end

    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (acc, rpfx, NODE (item, vec)) =
        M.foldli (fn (x, n, acc) => foldli_helper f (acc, x :: rpfx, n))
                 (case item of
                      NONE => acc
                    | SOME v => f (rev rpfx, v, acc))
                 vec
                      
    fun foldli f acc t =
        case t of
            EMPTY => acc
          | POPULATED n => foldli_helper f (acc, [], n)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch' f acc (node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (acc, rpfx, n, []) = foldli_helper f (acc, rpfx, n)
              | fold' (acc, rpfx, NODE (item, vec), x::xs) =
                case M.find (vec, x) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: rpfx, nsub, xs)
        in
            fold' (acc, [], node, e)
        end

    fun foldliPrefixMatch f acc (trie, e) =
        case trie of
            EMPTY => acc
          | POPULATED n => foldliPrefixMatch' f acc (n, e)
            
    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch' f acc (node, p) =
        let fun fold' (acc, rpfx, NODE (NONE, _), []) = acc
              | fold' (acc, rpfx, NODE (SOME v, _), []) = f (rev rpfx, v, acc)
              | fold' (acc, rpfx, NODE (_, vec), NONE::xs) =
                M.foldli (fn (x, n, acc) => fold' (acc, x :: rpfx, n, xs))
                         acc vec
              | fold' (acc, rpfx, NODE (_, vec), (SOME x)::xs) =
                case M.find (vec, x) of
                    NONE => acc
                  | SOME nsub => fold' (acc, x :: rpfx, nsub, xs)
        in
            fold' (acc, [], node, p)
        end

    fun foldliPatternMatch f acc (trie, p) =
        case trie of
            EMPTY => acc
          | POPULATED node => foldliPatternMatch' f acc (node, p)
            
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))

    fun prefixOf (trie, e) = 
        let fun prefix' (best, acc, n as NODE (item, vec), x::xs) =
                let val best = case item of
                                   NONE => best
                                 | SOME _ => acc
                in
                    prefix' (best,
                             x :: acc,
                             case M.find (vec, x) of
                                 NONE => newNode ()
                               | SOME nsub => nsub,
                             xs)
                end
              | prefix' (best, acc, NODE (SOME _, _), []) = acc
              | prefix' (best, acc, NODE (NONE, _), []) = best
        in
            case trie of
                EMPTY => []
              | POPULATED node => rev (prefix' ([], [], node, e))
        end

end

