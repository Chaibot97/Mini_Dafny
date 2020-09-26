#!/bin/bash
find ../Benchmarks -name "*.imp" | while read t
do
    echo Testing file "$t"
    output=$(cabal run vcgen "$t")
    if [ "$?" -ne "0" ]
    then
        echo "Failed"
        echo "$output"
    else
        echo "Success"
    fi 
done
