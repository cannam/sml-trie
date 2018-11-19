
signature STRING_TRIE = sig
    include PATTERN_MATCH_TRIE
    where type entry = string
    where type element = char
end

structure StringMTrie :> STRING_TRIE = PatternMatchTrieFn(StringMTrieMap)
structure StringATrie :> STRING_TRIE = PatternMatchTrieFn(StringATrieMap)
structure StringBTrie :> STRING_TRIE = PatternMatchTrieFn(StringBTrieMap)

structure StringTrie = StringATrie
