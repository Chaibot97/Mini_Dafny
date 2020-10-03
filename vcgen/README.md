Project1: Veriﬁcation Condition Generation  
Author: Lizhou Cai, Junrui Liu  
===  

## VCGen
To run the VCGen, installation of ___Haskell___ and ___z3___ are required.

If both of them are installed, `cd` to `/vcgen` and run
    
    ./vcgen.sh [path_to_imp]
    
will print out either 
* “verified” if the imp program is valid 
* “Not verified” if the program is invalid

To avoid z3 to freeze, 3 secound timeout is set.

We are currently using z3 version 4.8.9 and it has some bugs that cause the program to hang indefinitely for some test case (e.g. gcd.imp).
However, the version of z3 used in <https://rise4fun.com/z3/> is working fine.

In this case, by running

    ./vcgen.sh [path_to_imp] -v > output.smt2

you will be able to get the intermidiate `not(WP)` in the _smt2_ format and copy and paste it into the oneline z3 solver to get the correct result. (unsat-> valid, sat ->invalid) 


---
## Benchmarking
Benchmarks given by TA can be found at the directory `/Benchmarks`, while orginal benchmarks are in `/Benchmarks_ORI`.

Test files are divided into two subdirectories `/Benchmarks_ORI/valid` and `/Benchmarks_ORI/invalid` accroding to their validity.

To run all the tests, simply run

    ./test.sh

* It wil output `ok` if its output is consistent with its the directory.
* It wil output `failed` otherwise.

(Test _gcd.imp_ will timeout because of  z3v4.8.9's bug, but all the other test will pass)




