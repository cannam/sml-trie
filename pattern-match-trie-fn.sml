
(* Copyright 2015-2021 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

functor PatternMatchTrieFn (M : PATTERN_MATCH_TRIE_MAP)
        :> PATTERN_MATCH_TRIE
               where type element = M.element where type entry = M.key where type trie = unit M.trie = struct

    structure T = TrieFn(M)
    open T

    type element = M.element
    type entry = M.key
    type pattern = element option list

    fun foldlPattern f acc (t, p) =
        M.foldliPattern (fn (k, v, acc) => f (k, acc)) acc (t, p)

    fun foldrPattern f acc (t, p) =
        M.foldriPattern (fn (k, v, acc) => f (k, acc)) acc (t, p)

    fun enumeratePattern (t, p) =
        M.foldriPattern (fn (k, v, acc) => k :: acc) [] (t, p)
                             
end
