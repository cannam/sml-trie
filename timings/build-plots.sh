#!/bin/bash

set -eu

containers="PersistentHashMap/string RedBlackMap/string HashTable/string PersistentArray/int Vector/int PersistentQueue/int Fifo/int List/int"

outdir=out_$$
mkdir -p "$outdir"

for compiler in polyml mlton ; do
    case "$compiler" in
        polyml) ../../sml-buildscripts/polybuild hash-performance-test.mlb ;;
        mlton) mlton hash-performance-test.mlb ;;
    esac
    for container in $containers ; do
        combo="$container/$compiler"
        for n in 1000 3000 10000 30000 100000 300000 1000000 3000000 10000000 ; do
            ./hash-performance-test "$n" "$container" | tee "$outdir/current"
            grep "^$container" "$outdir/current" | sed 's/ | /|/g' |
                while IFS="|" read _c test _r _n _t kps ; do
                    testfile="$outdir"/$(echo "$test" | sed 's/ /_/g').csv
                    if test -f "$testfile" && grep -q "^$combo," "$testfile" ; then
                        perl -i -p -e 's|^('"$combo"'.*)$|$1,'"$kps"'|' "$testfile"
                    else
                        echo "$combo,$kps" >> "$testfile"
                    fi
                done
        done
    done
done

