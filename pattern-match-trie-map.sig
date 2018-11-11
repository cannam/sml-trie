
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature PATTERN_MATCH_TRIE_MAP = sig

    (* This is a trie that can do pattern matches as well as prefix
       matches. Say you have a trie containing some (conceptual)
       list/string entries matching ABB, ABC, and BBC. With the plain
       trie you can match e.g. prefix AB to return ABB and ABC. With a
       pattern-match trie you can alternatively provide a query list
       like [NONE, SOME B, NONE] to return all entries having three
       elements with B in the middle, here all three of the listed
       entries.

       This differs from the plain trie not only because of the
       additional pattern match function, but also because the type of
       the individual node elements is exposed, whereas TRIE uses an
       atomic entry type. *)
    
    include TRIE_MAP

    type element
    type pattern = element option list

    (* Return all the key-value pairs in the trie that match the given
       pattern, in sort order. Will only return entries with exactly
       the same number of elements as values in the pattern *)
    val patternMatch : ('a trie * pattern) -> (key * 'a) list

    (* Fold over all the key-value pairs in the trie that match the
       given pattern, in sort order. Will only return entries with
       exactly the same number of elements as values in the pattern *)
    val foldliPatternMatch : (key * 'a * 'b -> 'b) -> 'b -> ('a trie * pattern) -> 'b
	    
end
