
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
                                                val maxOrd = Char.maxOrd
                                                val toString = Char.toString
                                                end))

