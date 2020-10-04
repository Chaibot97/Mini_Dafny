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
elif [ "$2" == "-vc" ]; then # print verification condition only
    echo "$vc"
else
    out=$(echo "$vc" | z3 -in -t:3000)
    if [ "$out" = unsat ]; then
        echo Verified
    elif [ "$out" = sat ]; then
        echo Not verified
    else
        echo "$out"
    fi
fi