#!/bin/bash

suites=(valid invalid)
for suite in "${suites[@]}"
do
  find ../Benchmarks/$suite -name "*.imp" | while read t
  do
    printf "$t >>> "
    out=$(./vcgen.sh "$t")
    if [[ $suite == valid && $out == "Verified" ]] || [[ $suite == invalid && $out == "Not verified" ]]; then
      echo ok
    else
      echo failed
    fi
  done
done

seq  -f "-" -s "" 80 
echo "Original Tests"

find ../Benchmarks_ORI -name "*.imp" | while read t
  do
    printf "$t >>> "
    out=$(./vcgen.sh "$t")
    if [[ $out = "Verified" ]]; then
      echo ok
    else
      echo failed
    fi
  done
