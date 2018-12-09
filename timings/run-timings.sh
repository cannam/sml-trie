#!/bin/bash

set -eu

#revisions="86c76acd5ad1 43affa1d8c55 d215ab4e4ea3 1d68b0542b9e af96c1ba753a f55eeb124661 471394ad63c5 c638f71c8229 7de34a26d314"
revisions="d215ab4e4ea3 f55eeb124661 471394ad63c5 c638f71c8229 7de34a26d314 3d93a7a5d14a bb4bba7d9e07"

if [ ! -d "sml-smlnj-containers" ]; then
    hg clone "https://bitbucket.org/cannam/sml-smlnj-containers"
fi

for revision in $revisions ; do

    name=$(case "$revision" in
               86c76acd5ad1) echo "string persistent hash map";;
               43affa1d8c55) echo "string persistent hash map";;
               *) echo "PersistentHashMap/string";;
           esac)
    
    dir="timing_$revision"
    hg clone -r"$revision" .. $dir
    pushd $dir/example

    echo "$revision: $(hg log -l1 | grep '^date' | sed 's/^[^ ]* *//')"
    
    echo
    echo "$revision - Poly/ML:"
    /bin/time -f '%M Kb' ../../../../sml-buildscripts/polyrun ./hash-performance-test.mlb 1000000 "$name" || echo "FAILED"

    echo
    mlton hash-performance-test.mlb
    echo "$revision - MLton:"
    /bin/time -f '%M Kb' ./hash-performance-test 1000000 "$name" || echo "FAILED"
    
    popd
    rm -rf "$dir"

    echo

done
