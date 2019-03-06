
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature LIST_TRIE_NODE_MAP = sig
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
                                          
functor ListTrieMapFn (M : LIST_TRIE_NODE_MAP)
	:> PATTERN_MATCH_TRIE_MAP
	       where type element = M.key where type key = M.key list = struct

    type element = M.key
    type key = element list
    type pattern = element option list

    datatype 'a node = TWIG of key * 'a  (* if key is empty, it's a leaf *)
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
            case (n, xx) of
                (TWIG ([], item), []) => TWIG ([], f' item)
              | (TWIG ([], item), xx) => update' (newBranch (SOME item),
                                                  xx, f)
              | (TWIG (kk, item), []) => update' (newBranch (SOME (f NONE)),
                                                  kk, fn _ => item)
              | (TWIG (kk, item), xx) =>
                (if kk = xx
                 then TWIG (kk, f' item)
                 else if hd kk = hd xx  (* e.g. adding XDEF next to XABC *)
                 then BRANCH (NONE,
                              M.update
                                  (M.new (), hd kk,
                                   fn _ => update' (update' (newBranch NONE,
                                                             tl xx, f),
                                                    tl kk, fn _ => item)))
                 else update' (update' (newBranch NONE,
                                        kk, fn _ => item),
                               xx, f))
              | (BRANCH (iopt, m), []) => BRANCH (SOME (f iopt), m)
              | (BRANCH (iopt, m), x::xs) =>
                BRANCH (iopt,
                        M.update (m, x,
                                  fn NONE => TWIG (xs, f NONE)
                                   | SOME nsub => update' (nsub, xs, f)))
        end

    fun update (EMPTY, xx, f) = POPULATED (update' (newBranch NONE, xx, f))
      | update (POPULATED n, xx, f) = POPULATED (update' (n, xx, f))

    fun insert (n, xx, v) =
        update (n, xx, fn _ => v)

    fun remove' (n, xx) =
        case (n, xx) of
            (TWIG ([], _), []) => newBranch NONE
          | (TWIG ([], _), _) => n
          | (TWIG (kk, item), xx) =>
            (if kk = xx
             then newBranch NONE
             else n)
          | (BRANCH (iopt, m), []) => BRANCH (NONE, m)
          | (BRANCH (iopt, m), x::xs) =>
            case M.find (m, x) of
                NONE => n
              | SOME nsub =>
                let val nsub' = remove' (nsub, xs)
                in
                    if isEmptyBranch nsub'
                    then BRANCH (iopt, M.remove (m, x))
                    else BRANCH (iopt, M.update (m, x, fn _ => nsub'))
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
        case (n, xx) of
            (TWIG ([], item), []) => SOME item
          | (TWIG ([], _), _) => NONE
          | (TWIG (kk, item), xx) =>
            (if kk = xx
             then SOME item
             else NONE)
          | (BRANCH (iopt, m), []) => iopt
          | (BRANCH (iopt, m), x::xs) =>
            case M.find (m, x) of
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
        let fun fold' (n, acc) =
                case n of
                    TWIG (_, item) => f (item, acc)
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
        case n of
            TWIG ([], item) => f (rev rpfx, item, acc)
          | TWIG (kk, item) => f ((rev rpfx) @ kk, item, acc)
          | BRANCH (iopt, m) =>
            M.foldli (fn (x, n, acc) => foldli_helper f (x :: rpfx, n, acc))
                     (case iopt of
                          NONE => acc
                        | SOME item => f (rev rpfx, item, acc))
                     m
                      
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
        let fun fold' (rpfx, n, [], acc) = foldli_helper f (rpfx, n, acc)
              | fold' (rpfx, n, xx, acc) =
                case n of
                    TWIG ([], item) => acc
                  | TWIG (kk, item) =>
                    (if isPrefixOf (xx, kk)
                     then foldli_helper f (rpfx, n, acc)
                     else acc)
                  | BRANCH (iopt, m) => 
                    case M.find (m, hd xx) of
                        NONE => acc
                      | SOME nsub =>
                        fold' ((hd xx) :: rpfx, nsub, (tl xx), acc)
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
        let fun fold' (rpfx, n, xx, acc) =
                case (n, xx) of
                    (BRANCH (NONE, _), []) => acc
                  | (BRANCH (SOME item, _), []) => f (rev rpfx, item, acc)
                  | (TWIG (_, item), []) => f (rev rpfx, item, acc)
                  | (TWIG ([], _), _) => acc
                  | (TWIG (kk, item), xx) =>
                    (if ListPair.allEq (fn (k, NONE) => true
                                         | (k, SOME x) => k = x) (kk, xx)
                     then f (rev rpfx @ kk, item, acc)
                     else acc)
                  | (BRANCH (_, m), NONE::xs) =>
                    M.foldli (fn (x, n, acc) => fold' (x :: rpfx, n, xs, acc))
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
                case (n, xx) of
                    (TWIG ([], _), _) => acc
                  | (TWIG (_, _), []) => acc
                  | (TWIG (kk, item), xx) =>
                    if kk = xx
                    then (rev kk) @ acc
                    else best
                  | (BRANCH (NONE, m), []) => best
                  | (BRANCH (SOME item, m), []) => acc
                  | (BRANCH (iopt, m), x::xs) =>
                    let val best = case iopt of
                                       NONE => best
                                     | SOME _ => acc
                    in
                        case M.find (m, x) of
                            NONE => best
                          | SOME nsub => prefix' (nsub, xs, best, x :: acc)
                    end
        in
            case trie of
                EMPTY => []
              | POPULATED node => rev (prefix' (node, e, [], []))
        end

end

