#!/bin/bash
if ! command -v z3 &> /dev/null; then
    echo "z3 could not be found"
    exit
fi
if ! command -v cabal &> /dev/null; then
    echo "cabal could not be found"
    exit
fi

vc=$(cabal -v0 new-run vcgen "$1")
if [ "$?" -ne "0" ]; then
    echo Failed
    echo "$vc"
elif [ "$2" == "-v" ]; then # print verification condition only
    echo "$vc"
else
    out=$(echo "$vc" | z3 -in -t:1000)
    if [ "$out" = sat ]; then
        echo Not verified
    elif [ "$out" = unsat ]; then
        echo Verified
    else
        echo Error
        echo "$out"
    fi
fi