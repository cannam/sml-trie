
Trie and persistent trie-based containers in Standard ML
========================================================

https://hg.sr.ht/~cannam/sml-trie

This is a trie library in Standard ML.

The library contains implementations of the following container
data structures:

   1. Trie and Trie Map
   2. Persistent Hash Map (implemented using bitmap tries)
   3. Persistent Array and Queue (implemented using bitmap tries)


Trie and Trie Map
-----------------

A trie is an ordered container. It stores a set of entries of a common
entry type, which is some sort of sequence (vector, list, string etc),
storing them in a tree structure according to their common prefixes,
branching where they diverge.

```
                          g - a - t - o - r*
                        /
        a* - l - l* - i
      /                 \
    .                     a - n - c - e* - s*
      \ 
        z - e - b - r - a*
```

(The asterisk shows nodes marked as entries in the trie rather than
just potential branches, so this trie contains the six string entries
"a", "all", "alligator", "alliance", "alliances", and "zebra".)

A plain trie just records the presence or absence of an entry in a
set, but a trie can also support a map container, in which each entry
(or key) has an associated value as well. A plain trie can be
expressed as a trie map whose value is of a valueless (unit) type.

As with a hash map or typical tree-map structure, membership testing
is generally fast. Unlike those, with a trie you can also quickly test
whether any prefix of a given entry appears in the trie, obtain the
longest such prefix, and find all entries with a given prefix. Like a
tree-backed container but not a hash table, a trie is ordered, and can
be enumerated in a fixed order, typically the sort order of the entry
type.

This library provides persistent or immutable trie containers, in the
sense that all updates return a separate trie, without modifying the
one passed in, but internally sharing any unmodified parts with it.

The main signatures are:

 * `TRIE` (`trie.sig`) - signature of a trie set container with
   arbitrary entry type

 * `TRIE_MAP` (`trie-map.sig`) - signature of a polymorphic-value trie
   map container with arbitrary key type

 * `PATTERN_MATCH_TRIE` (`pattern-match-trie.sig`) - signature
   extending `TRIE` with the capability of matching sequences with
   wildcards in them. Unlike `TRIE`, this exposes the types of the
   individual elements in each entry (e.g. `char`, for a trie with
   string entries)

 * `PATTERN_MATCH_TRIE_MAP` (`pattern-match-trie-map.sig`) - signature
   extending `TRIE_MAP` with the capability of matching sequences with
   wildcards in them, like `PATTERN_MATCH_TRIE`

There is a single core trie map implementation provided: `TrieMapFn`,
in `trie-map-fn.sml`. This is a functor parameterised by the node map
type for the trie (which determines how sub-node relationships are
stored) and the external key and entry types (which determine the API
for the trie). All of the external concrete trie-map and trie
structures are implemented using `TrieMapFn` internally.

These functors then adapt trie-maps into set-type trie structures:

 * `TrieFn` (`trie-fn.sml`) - a functor that turns a `TRIE_MAP` into a
   `TRIE`

 * `PatternMatchTrieFn` (`pattern-match-trie-fn.sml`) - a functor that
   turns a `PATTERN_MATCH_TRIE_MAP` into a `PATTERN_MATCH_TRIE`

The following external interfaces are then provided in list-tries.sml:

 * `ListMTrieMapFn` - a functor that takes a comparable element type
   and implements a `PATTERN_MATCH_TRIE_MAP` in which the entry type
   is a list of that element. The implementation uses a red-black tree
   map (from the SML/NJ library) at every trie node. This is better
   suited to wide, shallow trie structures than narrow, deep ones, but
   can be surprisingly efficient in both time and space. Insertion and
   enumeration are relatively cheap and lookup is relatively more
   expensive.

 * `ListATrieMapFn` - a functor that takes an element type that can be
   mapped compactly onto a small integer range, and implements a
   `PATTERN_MATCH_TRIE_MAP` in which the entry type is a list of that
   element. The implementation uses an array (or rather a vector) at
   each node, extending the vector to span the range of inserted
   values. This may be better suited than `ListMTrieMapFn` to narrow,
   deep structures with small numbers of possible values at each
   node. Lookups are relatively cheap and insertion is relatively more
   expensive.

 * `ListBTrieMapFn` - a functor that takes an element type that can be
   mapped compactly onto a small integer range, and implements a
   `PATTERN_MATCH_TRIE_MAP` in which the entry type is a list of that
   element. The implementation uses a bitmap-compressed semi-sparse
   array at each node. This is only suited to entries whose elements
   map sparsely into a small range.

 * `ListMTrieFn`, `ListATrieFn`, `ListBTrieFn` (`list-tries.sml`) -
   shorthands that turn an element type directly into a trie set (the
   set equivalents of `*TrieMapFn`)

For each of these tries with list entry types, an equivalent trie with
a vector entry type is also provided, in `vector-tries.sml`:

 * `VectorMTrieMapFn`, `VectorATrieMapFn`, `VectorBTrieMapFn` - vector
   equivalents of `ListMTrieMapFn`, `ListATrieMapFn`, `ListBTrieMapFn`

 * `VectorMTrieFn`, `VectorATrieFn`, `VectorBTrieFn` - vector
   equivalents of `ListMTrieFn`, `ListATrieFn`, `ListBTrieFn`

Finally there are specialisations for tries with string entries, found
in `string-tries.sml`:

 * `StringMTrieMap`, `StringATrieMap`, `StringBTrieMap` - trie-maps
   with string key type

 * `StringMTrie`, `StringATrie`, `StringBTrie` - trie sets with string
   key type

 * `StringTrieMap`, `StringTrie` - aliases for `StringMTrieMap`,
   `StringMTrie`


Persistent Hash Map
-------------------

This library contains an implementation of a persistent hash map
structure. This is a hash map in which every insertion or removal
returns a separate hash map, without modifying the one passed in, but
internally sharing any unmodified parts with it.

Like a classic hash table, the persistent hash map is unordered: keys
can be enumerated, but not in any guaranteed order, and do not need to
be comparable. It also requires that a hash function be provided for
the key type.

The implementation is a hash-array-mapped trie (HAMT) somewhat like
that popularised by the Clojure language. Hashes are 32-bit words and
collisions are handled using simple list chaining.

The main signature is:

 * PERSISTENT_HASH_MAP (persistent-hash-map.sig) - signature of a
   polymorphic-value hash map container with a fixed key type

And this is implemented by

 * `PersistentHashMapFn` (`persistent-hash-map-fn.sml`) - a functor
   that takes a hash key type and hash function and implements a
   `PERSISTENT_HASH_MAP` with that key type

There is a specialisation for string keys:

 * `StringHashMap` (`string-hash-map.sml`) - a `PERSISTENT_HASH_MAP`
   with string keys


Persistent Array and Queue
--------------------------

This library contains implementations of persistent array and
double-ended queue containers. These are immutable containers like the
Basis vector with the addition of update, append, and pop-from-end
operations which return new (partly shared) containers. The queue
supports all of the same operations plus prepend and pop-from-start,
but its plain iteration (`foldl`) is not as fast as the array. For
both containers, all of the update, append, prepend, and pop
operations take time independent of the container size.

Note that these containers are competitive only if both immutability
and general updating are desired. A vector is faster to tabulate and
iterate over, a mutable array is faster to update existing elements
of, and a fifo implemented with two lists is a faster persistent queue
if updating within the queue is not needed (as it usually isn't).

The implementations use an array-mapped-trie somewhat like that
popularised by the Clojure language. Both containers are limited in
size to 2^32 elements or the maximum size of `Int.int`, whichever is
smaller. The former is an implementation limit, the latter an artifact
of the choice of API. For the queue, the limit is of current queue
length, not of the total number of append/pop cycles.

The main signatures are:

 * `PERSISTENT_ARRAY` (`persistent-array.sig`) - signature of a
   polymorphic-value persistent array container

 * `PERSISTENT_ARRAY_SLICE` (`persistent-array-slice.sig`) - signature
   of a slice into a `PERSISTENT_ARRAY`

 * `PERSISTENT_QUEUE` (`persistent-queue.sig`) - signature of a
   polymorphic-value persistent queue container

And these are implemented by

 * `PersistentArray` (`persistent-array.sml`) - a polymorphic-value
   persistent array container

 * `PersistentArraySlice` (`persistent-array-slice.sml`) - slice into
   a `PersistentArray`

 * `PersistentQueue` (`persistent-queue.sml`) - a polymorphic-value
   persistent queue container


To Build & Test
---------------

Basic unit tests are provided. With the MLton compiler, run

```
 $ mlton test.mlb && ./test
```

To use in other projects, include `trie.mlb` from your MLB file.


Author
------

Copyright 2015-2021 Chris Cannam.
MIT/X11 licence. See the file `COPYING` for details.

