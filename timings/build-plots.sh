#!/bin/bash

set -eu

containers="PersistentHashMap/string RedBlackMap/string HashTable/string PersistentArray/int Vector/int Array/int PersistentQueue/int Fifo/int"

outdir=out_$(date +%s)
mkdir -p "$outdir"

for compiler in polyml mlton ; do
    case "$compiler" in
        polyml) ../../sml-buildscripts/polybuild hash-performance-test.mlb ;;
        mlton) mlton -runtime 'copy-generational-ratio 10.0' -runtime 'ram-slop 0.8' hash-performance-test.mlb ;;
    esac
    for container in $containers ; do
        combo="$container/$compiler"
        for n in 1000 3000 10000 30000 100000 300000 1000000 1500000 3000000 6000000 10000000 ; do

            memory=$(/bin/time -f '%M' ./hash-performance-test "$n" "$container/memory" 2>&1 > /dev/null)
            testfile="$outdir"/memory.csv
            if test -f "$testfile" && grep -q "^$combo," "$testfile" ; then
                perl -i -p -e 's|^('"$combo"'.*)$|$1,'"$memory"'|' "$testfile"
            else
                echo "$combo,$memory" >> "$testfile"
            fi
            
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

