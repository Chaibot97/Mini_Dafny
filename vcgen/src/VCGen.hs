module Main where

import Language
import Parser.Parser

import System.Environment

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as) 
    print $ parseProg prog
