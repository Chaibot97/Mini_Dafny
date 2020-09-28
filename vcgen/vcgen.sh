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
else
    out=$(echo "$vc" | z3 -in -t:2000)
    if [ "$out" = sat ]; then
        echo invalid
    elif [ "$out" = unsat ]; then
        echo valid
    else
        echo Error
        echo "$out"
    fi
fi