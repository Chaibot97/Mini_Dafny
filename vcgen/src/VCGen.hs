module Main where

import Language
import Parser.Parser

import System.Environment

data GuardedCommand = GCAssert Assertion
                    | GCAssume Assertion
                    | GCHavoc  Name
                    | GCChoice GCBlock GCBlock

type GCBlock = [GuardedCommand]

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as) 
    print $ parseProg prog
