
(* Copyright 2015-2016 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

signature STRING_TRIE = sig
    include PATTERN_MATCH_TRIE
    where type entry = string
    where type element = char
end

signature CHAR_LIST_TRIE = sig
    include PATTERN_MATCH_TRIE
    where type entry = char list
    where type element = char
end
                            
functor StringTrieFn (T : CHAR_LIST_TRIE) :> STRING_TRIE = struct

    type t = T.t
    type trie = t
    type element = char
    type pattern = char option list
    type entry = string

    val empty = T.empty

    fun add (trie, s) =
        T.add (trie, String.explode s)

    fun contains (trie, s) =
        T.contains (trie, String.explode s)
                         
    fun remove (trie, s) =
        T.remove (trie, String.explode s)

    fun foldl f acc trie =
        T.foldl (fn (e, acc) => f (String.implode e, acc))
                           acc trie

    fun enumerate trie =
        List.map String.implode (T.enumerate trie)

    fun foldlPrefixMatch f acc (trie, s) =
        T.foldlPrefixMatch (fn (e, acc) => f (String.implode e, acc))
                                 acc (trie, String.explode s)
                 
    fun prefixMatch (trie, s) =
        List.map String.implode (T.prefixMatch (trie, String.explode s))

    fun prefixOf (trie, s) =
        String.implode (T.prefixOf (trie, String.explode s))

    fun foldlPatternMatch f acc (trie, p) =
        T.foldlPatternMatch (fn (e, acc) => f (String.implode e, acc))
					 acc (trie, p)
                 
    fun patternMatch (trie, p) =
        List.map String.implode (T.patternMatch (trie, p))
                 
end
    
structure StringMapTrie = StringTrieFn
                              (ListMapTrieFn(struct
				              type t = char
				              val compare = Char.compare
				              end))

structure StringArrayTrie = StringTrieFn
                                (ListArrayTrieFn(struct
				                  type t = char
                                                  val ord = Char.ord
                                                  val invOrd = Char.chr
                                                  val maxOrd = Char.maxOrd
                                                  end))

structure StringTrie = StringMapTrie
