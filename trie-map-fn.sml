
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature TRIE_NODE_MAP = sig
    eqtype key (*!!! would be ideal if didn't have to be eqtype - that is a disadvantage of the twig approach at the mo *)
    type 'a map
    val new : unit -> 'a map
    val isEmpty : 'a map -> bool
    val find : 'a map * key -> 'a option
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val update : 'a map * key * ('a option -> 'a) -> 'a map
    val remove : 'a map * key -> 'a map
end

signature TRIE_KEY = sig
    eqtype element
    type key
    val isEmpty : key -> bool
    val head : key -> element
    val tail : key -> key
    val explode : key -> element list
    val implode : element list -> key
    val equal : key * key -> bool
end

signature TRIE_MAP_FN_ARG = sig
    eqtype element
    type key
    structure M : TRIE_NODE_MAP where type key = element
    structure K : TRIE_KEY where type element = element where type key = key
end

functor TrieMapFn (A : TRIE_MAP_FN_ARG)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = A.K.element where type key = A.K.key = struct

    structure M = A.M
    structure K = A.K
                      
    type element = K.element
    type key = K.key
    type pattern = element option list

    datatype 'a node = LEAF of 'a
                     | TWIG of key * 'a (* key is nonempty, else it's a leaf *)
                     | BRANCH of 'a option * 'a node M.map

    datatype 'a trie = EMPTY
                     | POPULATED of 'a node
        
    val empty = EMPTY

    fun isEmpty EMPTY = true
      | isEmpty _ = false

    fun newBranch opt = BRANCH (opt, M.new ())

    fun isEmptyBranch (BRANCH (NONE, m)) = M.isEmpty m
      | isEmptyBranch _ = false

    fun update' (n, xx, f : 'a option -> 'a) =
        let fun f' item = f (SOME item)
        in
            case (n, K.isEmpty xx) of
                (LEAF item, true) => LEAF (f' item)
              | (LEAF item, false) => update' (newBranch (SOME item),
                                               xx, f)
              | (TWIG (kk, item), true) => update' (newBranch (SOME (f NONE)),
                                                    kk, fn _ => item)
              | (TWIG (kk, item), false) =>
                (if K.equal (kk, xx)
                 then TWIG (kk, f' item)
                 else if K.head kk = K.head xx (* e.g. adding XDEF next to XABC *)
                 then BRANCH (NONE,
                              M.update
                                  (M.new (), K.head kk,
                                   fn _ => update' (update' (newBranch NONE,
                                                             K.tail xx, f),
                                                    K.tail kk, fn _ => item)))
                 else update' (update' (newBranch NONE,
                                        kk, fn _ => item),
                               xx, f))
              | (BRANCH (iopt, m), true) => BRANCH (SOME (f iopt), m)
              | (BRANCH (iopt, m), false) =>
                BRANCH (iopt,
                        M.update (m, K.head xx,
                                  fn SOME nsub => update' (nsub, K.tail xx, f)
                                   | NONE =>
                                     let val xs = K.tail xx
                                     in
                                         if K.isEmpty xs
                                         then LEAF (f NONE)
                                         else TWIG (xs, f NONE)
                                     end))
        end

    fun update (EMPTY, xx, f) = POPULATED (update' (newBranch NONE, xx, f))
      | update (POPULATED n, xx, f) = POPULATED (update' (n, xx, f))

    fun insert (n, xx, v) =
        update (n, xx, fn _ => v)

    fun remove' (n, xx) =
        case (n, K.isEmpty xx) of
            (LEAF item, true) => newBranch NONE
          | (LEAF _, _) => n
          | (TWIG (kk, item), _) => (if K.equal (kk, xx)
                                     then newBranch NONE
                                     else n)
          | (BRANCH (iopt, m), true) => BRANCH (NONE, m)
          | (BRANCH (iopt, m), false) =>
            let val x = K.head xx
            in
                case M.find (m, x) of
                    NONE => n
                  | SOME nsub =>
                    let val nsub' = remove' (nsub, K.tail xx)
                    in
                        if isEmptyBranch nsub'
                        then BRANCH (iopt, M.remove (m, x))
                        else BRANCH (iopt, M.update (m, x, fn _ => nsub'))
                    end
            end
                        
    fun remove (EMPTY, _) = EMPTY
      | remove (POPULATED n, xx) =
        let val n' = remove' (n, xx)
        in
            if isEmptyBranch n'
            then EMPTY
            else POPULATED n'
        end

    fun find' (n, xx) =
        case (n, K.isEmpty xx) of
            (LEAF item, true) => SOME item
          | (LEAF _, _) => NONE
          | (TWIG (kk, item), _) => (if K.equal (kk, xx)
                                     then SOME item
                                     else NONE)
          | (BRANCH (iopt, m), true) => iopt
          | (BRANCH (iopt, m), false) =>
            case M.find (m, K.head xx) of
                NONE => NONE
              | SOME nsub => find' (nsub, K.tail xx)

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
        let fun fold' (n, acc) =
                case n of
                    LEAF item => f (item, acc)
                  | TWIG (kk, item) => f (item, acc)
                  | BRANCH (iopt, m) =>
                    M.foldl fold' (case iopt of
                                       NONE => acc
                                     | SOME item => f (item, acc))
                            m
        in
            case t of
                EMPTY => acc
              | POPULATED n => fold' (n, acc)
        end

    (* rpfx is reversed prefix built up so far (using cons) *)
    fun foldli_helper f (rpfx, n, acc) =
        let fun f' (pfx, item, acc) = f (K.implode pfx, item, acc)
        in
            case n of
                LEAF item => f' (rev rpfx, item, acc)
              | TWIG (kk, item) => f' ((rev rpfx) @ (K.explode kk), item, acc)
              | BRANCH (iopt, m) =>
                M.foldli (fn (x, n, acc) => foldli_helper f (x :: rpfx, n, acc))
                         (case iopt of
                              NONE => acc
                            | SOME item => f' (rev rpfx, item, acc))
                         m
        end
                      
    fun foldli f acc t =
        case t of
            EMPTY => acc
          | POPULATED n => foldli_helper f ([], n, acc)

    fun enumerate trie =
        rev (foldli (fn (k, v, acc) => (k, v) :: acc) [] trie)

    fun isPrefixOf ([], yy) = true
      | isPrefixOf (xx, []) = false
      | isPrefixOf (x::xs, y::ys) = x = y andalso isPrefixOf (xs, ys)
            
    fun foldliPrefixMatch' f acc (node, e) = 
        (* rpfx is reversed prefix built up so far (using cons) *)
        let fun fold' (rpfx, n, xx, acc) =
                if K.isEmpty xx
                then foldli_helper f (rpfx, n, acc)
                else
                    case n of
                        LEAF item => acc
                      | TWIG (kk, item) =>
                        (if isPrefixOf (K.explode xx, K.explode kk)
                         then foldli_helper f (rpfx, n, acc)
                         else acc)
                      | BRANCH (iopt, m) => 
                        case M.find (m, K.head xx) of
                            NONE => acc
                          | SOME nsub =>
                            fold' ((K.head xx) :: rpfx, nsub, (K.tail xx), acc)
        in
            fold' ([], node, e, acc)
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
        let fun f' (pfx, item, acc) = f (K.implode pfx, item, acc)
            fun fold' (rpfx, n, xx, acc) =
                case (n, xx) of
                    (LEAF item, []) => f' (rev rpfx, item, acc)
                  | (TWIG (kk, item), []) => f' (rev rpfx, item, acc)
                  | (BRANCH (NONE, _), []) => acc
                  | (BRANCH (SOME item, _), []) => f' (rev rpfx, item, acc)
                  | (LEAF _, xx) => acc
                  | (TWIG (kk, item), xx) =>
                    (if ListPair.allEq (fn (k, NONE) => true
                                         | (k, SOME x) => k = x)
                                       (K.explode kk, xx)
                     then f' (rev rpfx @ K.explode kk, item, acc)
                     else acc)
                  | (BRANCH (_, m), NONE::xs) =>
                    M.foldli (fn (x, n, acc) =>
                                 fold' (x :: rpfx, n, xs, acc))
                             acc m
                  | (BRANCH (_, m), (SOME x)::xs) =>
                    case M.find (m, x) of
                        NONE => acc
                      | SOME nsub => fold' (x :: rpfx, nsub, xs, acc)
        in
            fold' ([], node, p, acc)
        end

    fun foldliPatternMatch f acc (trie, p) =
        case trie of
            EMPTY => acc
          | POPULATED node => foldliPatternMatch' f acc (node, p)
            
    fun patternMatch (trie, p) =
        rev (foldliPatternMatch (fn (k, v, acc) => (k, v) :: acc) [] (trie, p))
                                          
    fun prefixOf (trie, e) =
        let fun prefix' (n, xx, best, acc) =
                if K.isEmpty xx
                then case n of
                         LEAF item => acc
                       | TWIG (kk, item) => acc
                       | BRANCH (NONE, m) => best
                       | BRANCH (SOME item, m) => acc
                else case n of
                         LEAF item => acc
                       | TWIG (kk, item) =>
                         if K.equal (kk, xx) orelse
                            isPrefixOf (K.explode kk, K.explode xx)
                         then rev (K.explode kk) @ acc
                         else best
                       | BRANCH (iopt, m) =>
                         let val (x, xs) = (K.head xx, K.tail xx)
                             val best = case iopt of NONE => best
                                                   | SOME _ => acc
                         in
                             case M.find (m, x) of
                                 NONE => best
                               | SOME nsub => prefix' (nsub, xs, best, x::acc)
                         end
        in
            K.implode
                (case trie of
                     EMPTY => []
                   | POPULATED node => rev (prefix' (node, e, [], [])))
        end

end
