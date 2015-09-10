module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>=
         \paths -> doctest $ "-XOverloadedStrings" : "-XScopedTypeVariables" : "-idist/build/autogen" : paths
