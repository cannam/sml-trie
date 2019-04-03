
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

functor PatternMatchTrieFn (M : PATTERN_MATCH_TRIE_MAP)
        :> PATTERN_MATCH_TRIE
               where type element = M.element where type entry = M.key where type trie = unit M.trie = struct

    structure T = TrieFn(M)
    open T

    type element = M.element
    type entry = M.key
    type pattern = element option list

    fun keysOf kvl =
        map (fn (k, v) => k) kvl

    fun patternMatch (t, p) =
        keysOf (M.patternMatch (t, p))

    fun foldlPatternMatch f acc (t, p) =
        M.foldliPatternMatch (fn (k, v, acc) => f (k, acc)) acc (t, p)
                             
end
