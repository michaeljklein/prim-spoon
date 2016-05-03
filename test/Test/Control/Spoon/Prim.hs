module Test.Control.Spoon.Prim where

import Control.Spoon             (teaspoon          )
import Control.Spoon.Prim        (primspoon, throws )
import Data.Maybe                (isJust            )
import System.IO.Error           (ioError, userError)
import System.IO.Unsafe          (unsafePerformIO   )
import Test.HUnit
import Test.QuickCheck           (quickCheck        )
import Test.QuickCheck.Arbitrary (Arbitrary(..)     )


testTrue :: (Arbitrary a, Show a) => (a -> Bool) -> Test
testTrue = test . quickCheck

testFalse :: (Arbitrary a, Show a) => (a -> Bool) -> Test
testFalse = testTrue . (not .)

testIsJust :: (Int -> Maybe Int) -> Test
testIsJust = testTrue . (isJust .)

unsafeIOError :: a
unsafeIOError = unsafePerformIO . ioError . userError $ "throws missed an ioError"

testTeaspoon :: Test
testTeaspoon =  test [ "Test teaspoon for undefined"  ~: Nothing ~=? teaspoon  (undefined :: Int),
                       "Test teaspoon for defined  "  ~: testIsJust  teaspoon
                     ]

testPrimspoon :: Test
testPrimspoon = test [ "Test primspoon for undefined" ~: Nothing ~=? primspoon (undefined :: Int),
                       "Test primspoon for defined  " ~: testIsJust  primspoon
                     ]

testThrows :: Test
testThrows =    test [ "Test throws for undefined   " ~: True    ~=? throws    (undefined :: Int),
                       "Test throws for ioError     " ~: True    ~=? throws    unsafeIOError,
                       "Test throws for defined     " ~: testFalse  (throws :: Int -> Bool)
                     ]


testControlSpoonPrim :: [Test]
testControlSpoonPrim =  [ testTeaspoon
                        , testPrimspoon
                        , testThrows
                        ]
