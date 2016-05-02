module Main where

import Bench.Control.Spoon.Prim (benchControlSpoonPrim)
import Criterion.Main           (defaultMain          )

main :: IO ()
main = defaultMain . concat $ [ benchControlSpoonPrim
                              ]
