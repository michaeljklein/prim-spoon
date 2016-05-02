module Test.Control.Spoon.Prim where

import Control.Spoon      (teaspoon  )
import Control.Spoon.Prim (primspoon )
import Test.HUnit
import Test.QuickCheck    (quickCheck)

testIsJust :: (Int -> Maybe Int) -> Test
testIsJust f = test $ quickCheck (\x -> Just x == f x)

testTeaspoon :: Test
testTeaspoon =  test [ "Test teaspoon for undefined"  ~: Nothing ~=? teaspoon  (undefined :: Int),
                       "Test teaspoon for defined  "  ~: testIsJust  teaspoon
                     ]

testPrimspoon :: Test
testPrimspoon = test [ "Test primspoon for undefined" ~: Nothing ~=? primspoon (undefined :: Int),
                       "Test primspoon for defined  " ~: testIsJust  primspoon
                     ]

testControlSpoonPrim :: [Test]
testControlSpoonPrim =  [ testTeaspoon
                        , testPrimspoon
                        ]
