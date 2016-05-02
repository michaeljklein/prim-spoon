module Main where

import Test.Control.Spoon.Prim (testControlSpoonPrim)
import Test.HUnit

doTests :: [[Test]] -> IO ()
doTests ts = print =<< (runTestTT. test . concat) ts

main :: IO ()
main = doTests  [ testControlSpoonPrim
                ]

