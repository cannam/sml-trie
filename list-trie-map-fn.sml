
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
                               
    datatype 'a node = NODE of 'a M.map * 'a node M.map

    datatype 'a trie = EMPTY |
                       TRIE of 'a option * 'a node

    val empty = EMPTY
                                                         
    fun newNode () = NODE (M.new (), M.new ())
        
    fun isEmptyNode (NODE (vm, nm)) = M.isEmpty vm andalso M.isEmpty nm
      | isEmptyNode _ = false

    fun isEmpty EMPTY = true
      | isEmpty _ = false

    fun update' (NODE (vm, nm), xx, f) =
        case xx of
            [] => raise Fail "Internal error: empty entry reached in update'"
          | [x] => NODE (M.update (vm, x, f), nm)
          | x::xs => NODE (vm, M.update (nm, x,
                                         fn NONE => update' (newNode (), xs, f)
                                          | SOME nsub => update' (nsub, xs, f)))

    fun update (EMPTY, xx, f) = update (TRIE (NONE, newNode ()), xx, f)
      | update (TRIE (v, n), [], f) = TRIE (SOME (f v), n)
      | update (TRIE (v, n), xx, f) = TRIE (v, update' (n, xx, f))

    fun insert (n, xx, v) =
        update (n, xx, fn _ => v)

    fun remove' (NODE (vm, nm), xx) =
        case xx of
            [] => raise Fail "Internal error: empty entry reached in remove'"
          | [x] => NODE (M.remove (vm, x), nm)
          | x::xs => case M.find (nm, x) of
                         NONE => NODE (vm, nm)
                       | SOME nsub =>
                         let val nsub' = remove' (nsub, xs)
                         in
                             if isEmptyNode nsub'
                             then NODE (vm, M.remove (nm, x))
                             else NODE (vm, M.update (nm, x, fn _ => nsub'))
                         end

    fun remove (EMPTY, _) = EMPTY
      | remove (TRIE (_, n), []) = TRIE (NONE, n)
      | remove (TRIE (v, n), xx) =
        let val n' = remove' (n, xx)
        in
            if isEmptyNode n' andalso v = NONE
            then EMPTY
            else TRIE (v, n')
        end

    fun find' (NODE (vm, nm), xx) =
        case xx of
            [] => raise Fail "Internal error: empty entry reached in find'"
          | [x] => M.find (vm, x)
          | x::xs => case M.find (nm, x) of
                         NONE => NONE
                       | SOME nsub => find' (nsub, xs)

    fun find (EMPTY, _) = NONE
      | find (TRIE (v, _), []) = v
      | find (TRIE (_, n), xx) = find' (n, xx)
                               
    fun lookup (t, k) =
        case find (t, k) of
            NONE => raise Subscript
          | SOME v => v
                              
    fun contains (t, k) =
        case find (t, k) of
            SOME _ => true
          | NONE => false
             
    fun foldl f acc t =
        let fun fold' (NODE (vm, nm), acc) =
                M.foldl fold'
                        (M.foldl f acc vm)
                        nm
        in
            case t of
                EMPTY => acc
              | TRIE (NONE, n) => fold' (n, acc)
              | TRIE (SOME v, n) => fold' (n, f (v, acc))
        end

    fun foldli_helper f (rpfx, NODE (vm, nm), acc) =
        M.foldli (fn (k, n, acc) => foldli_helper f (k :: rpfx, n, acc))
                 (M.foldli (fn (k, v, acc) => f (k :: rpfx, v, acc))
                           acc vm)
                 nm
            
    fun foldli f acc t =
        case t of
            EMPTY => acc
          | TRIE (NONE, n) => foldli_helper f ([], n, acc)
          | TRIE (SOME v, n) => foldli_helper f ([], n, f ([], v, acc))

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun foldliPrefixMatch' f acc (node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (rpfx, n, acc, []) = foldli_helper f (rpfx, n, acc)
              | fold' (rpfx, NODE (vm, nm), acc, [x]) =
                (case M.find (vm, x) of
                     NONE => acc
                   | SOME v => f (x :: rpfx, v, acc))
              | fold' (rpfx, NODE (vm, nm), acc, x::xs) =
                (case M.find (nm, x) of
                     NONE => acc
                   | SOME nsub => fold' (x :: rpfx, nsub, acc, xs))
        in
            fold' ([], node, acc, e)
        end

    fun foldliPrefixMatch f acc (t, e) =
        case t of
            EMPTY => acc
          | TRIE (NONE, n) => foldliPrefixMatch' f acc (n, e)
          | TRIE (SOME v, n) =>
            case e of
                [] => foldliPrefixMatch' f (f ([], v, acc)) (n, e)
              | _ => foldliPrefixMatch' f acc (n, e)
                                        
    fun foldlPrefixMatch f acc (trie, e) =
        foldliPrefixMatch (fn (k, v, acc) => f (v, acc)) acc (trie, e)
            
    fun prefixMatch (trie, e) =
        rev (foldliPrefixMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, e))

    fun foldliPatternMatch' f acc (node, p) =
        let fun fold' (rpfx, n, acc, []) = acc
              | fold' (rpfx, NODE (vm, nm), acc, NONE::xs) =
                M.foldli (fn (k, n, acc) => fold' (k :: rpfx, n, acc, xs))
                         (M.foldli (fn (k, v, acc) => f (k :: rpfx, v, acc))
                                   acc vm)
                         nm
              | fold' (rpfx, NODE (vm, nm), acc, (SOME x)::xs) =
                M.foldli (fn (k, n, acc) => fold' (k :: rpfx, n, acc, xs))
                         (case M.find (vm, x) of
                              NONE => acc
                            | SOME v => f (x :: rpfx, v, acc))
                         nm
        in
            fold' ([], node, acc, p)
        end

    fun foldliPatternMatch f acc (t, p) =
        case (t, p) of
            (EMPTY, _) => acc
          | (TRIE (NONE, n), []) => acc
          | (TRIE (SOME v, n), []) => f ([], v, acc)
          | (TRIE (_, n), p) => foldliPatternMatch' f acc (n, p)
            
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

