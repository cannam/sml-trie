
(* Copyright 2015-2018 Chris Cannam.
   MIT/X11 licence. See the file COPYING for details. *)

structure StringMTrieMap = StringTrieMapFn
                               (ListMTrieMapFn(struct
				                type t = char
				                val compare = Char.compare
				                end))

structure StringATrieMap = StringTrieMapFn
                               (ListATrieMapFn(struct
				                type t = char
                                                val ord = Char.ord
                                                val invOrd = Char.chr
                                                end))

structure StringBTrieMap = StringTrieMapFn
                               (ListBTrieMapFn(struct
				                type t = char
                                                val ord = Char.ord
                                                val invOrd = Char.chr
                                                val maxOrd = Char.maxOrd
                                                end))

structure StringTrieMap = StringATrieMap

