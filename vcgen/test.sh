#!/bin/bash

suites=("../Benchmarks" "../Benchmarks_ORI")
cases=(valid invalid)
valid_msg="Verified"
invalid_msg="Not verified"
for suite in "${suites[@]}"; do
  seq  -f "-" -s "" 50
  echo "$suite"
  for case in "${cases[@]}"; do
    find "$suite"/$case -name "*.imp" | while read t; do
      printf "$t >>> "
      out=$(./vcgen.sh "$t")
      if [[ $case == valid && "$out" == "$valid_msg" ]] || [[ "$case" == invalid && "$out" == "$invalid_msg" ]]; then
        echo ok
      else
        echo failed / timeout
      fi
    done
  done
done