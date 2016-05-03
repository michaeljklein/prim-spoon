{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Bench.Control.Spoon.Prim (benchControlSpoonPrim) where

import Criterion.Main
import Control.Spoon      (teaspoon)
import Data.Maybe         (isJust)
import GHC.Prim           (RealWorld, State#, catch#, realWorld#, seq#)
import Control.Spoon.Prim
import System.IO.Unsafe   (unsafeDupablePerformIO)
import System.Mem.StableName


-- | Control mehod for benchmarking (appx. lower bound for a functional solution)
justToJust :: Maybe a -> Maybe (Maybe a)
justToJust y@(Just _) = Just y
justToJust Nothing    = Nothing

-- | This is `snd`, but unboxed
snd# :: (# State# RealWorld, b #) -> b
{-# INLINE snd# #-}
snd# (# _, y #) = y

-- | `snd#`, but more type specific
unpackMaybe :: (# State# RealWorld, Maybe a #) -> Maybe a
unpackMaybe (# _, x #) = x

-- | This is equivalent to @\_ _ -> (# realWorld#, True #)@, but needs a
-- specific type signature, because polymorphic unboxed types are not yet
-- supported by GHC
exceptionToTrue :: a -> State# RealWorld -> (# State# RealWorld, Bool #)
{-# INLINE exceptionToTrue #-}
exceptionToTrue _ _ = (# realWorld#, True #)


-- | Control method for benchmarking (appx. lower bound for output of a function)
catchMethod0 :: a -> Bool
catchMethod0 = const False

-- | This, and the following methods, are used for comparison to ensure the
-- prim-spoon methods are high-performance
catchMethod1 :: a -> Maybe a
{-# INLINE catchMethod1 #-}
catchMethod1 x = let r = realWorld# in let (# _, v #) = catch# (\s -> let (# t, y #) = (seq# x) s in (# t, Just y #)) (\_ _ -> (# r, Nothing #)) r in v

catchMethod2 :: a -> Maybe a
{-# INLINE catchMethod2 #-}
catchMethod2 x = unpackMaybe (catch# ((\f s -> (\(# t, y #) -> (# t, Just y #)) (f s)) (seq# x)) (\_ _ -> (# realWorld#, Nothing #)) realWorld#)

catchMethod3 :: a -> Maybe a
{-# INLINE catchMethod3 #-}
catchMethod3 x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, y #) -> (# t, Just y #)) (f s)) (seq# x)) (\_ _ -> (# realWorld#, Nothing #)) realWorld#)

catchMethod4 :: a -> Maybe a
{-# INLINE catchMethod4 #-}
catchMethod4 x = let r = realWorld# in (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, y #) -> (# t, Just y #)) (f s)) (seq# x)) (\_ _ -> (# r, Nothing #)) r)

catchMethod5 :: a -> Maybe a
{-# INLINE catchMethod5 #-}
catchMethod5 x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, y #) -> (# t, Just y #)) (f s)) (seq# x)) (\_ _ -> (# realWorld#, Nothing #)) realWorld#)

-- | This method *only* supports `Prelude.undefined` as the thrown error
-- (`unsafeDupablePerformIO` doesn't work with parallelism, but neither
-- does `makeStableName`)
catchMethod6 :: a -> Maybe a
{-# INLINE catchMethod6 #-}
catchMethod6 x = if eqStableName undefName . unsafeDupablePerformIO . makeStableName $ x
                    then Nothing
                    else Just x
  where
    undefName :: forall t. StableName t
    undefName = unsafeDupablePerformIO . makeStableName $ undefined


-- | This, and the following methods, are used to ensure the performance of
-- `throws`
throwsMethod0 :: a -> Bool
{-# INLINE throwsMethod0 #-}
throwsMethod0   = isJust . primspoon

throwsMethod1 :: a -> Bool
{-# INLINE throwsMethod1 #-}
throwsMethod1 x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, _ #) -> (# t, False #)) (f s)) (seq# x)) (\_ _ -> (# realWorld#, True #)) realWorld#)

throwsMethod2 :: a -> Bool
{-# INLINE throwsMethod2 #-}
throwsMethod2 x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, _ #) -> (# t, False #)) (f s)) (seq# x)) exceptionToTrue realWorld#)

throwsMethod3 :: a -> Bool
{-# INLINE throwsMethod3 #-}
throwsMethod3 x = snd# (catch# ((\f s -> (\(# t, _ #) -> (# t, False #)) (f s)) (seq# x)) (\_ _ -> (# realWorld#, True #)) realWorld#)

throwsMethod4 :: a -> Bool
{-# INLINE throwsMethod4 #-}
throwsMethod4 x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# _, _ #) -> (# realWorld#, False #)) (f s)) (seq# x)) (\_ _ -> (# realWorld#, True #)) realWorld#)

throwsMethod5 :: a -> Bool
{-# INLINE throwsMethod5 #-}
throwsMethod5 x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, _ #) -> (# t, False #)) (f s)) (seq# x)) (\_ u -> (# u, True #)) realWorld#)



-- | Benchmark the different options, with controls for comparison
benchControlSpoonPrim :: [Benchmark]
benchControlSpoonPrim = [
  bgroup "teaspoon            " [ bench "Check undefined" $ whnf teaspoon     undefined
                                , bench "Check defined"   $ whnf teaspoon     False
                                ],
  bgroup "const (control)     " [ bench "Check undefined" $ whnf catchMethod0 undefined
                                , bench "Check defined"   $ whnf catchMethod0 False
                                ],
  bgroup "Just (control)      " [
                                  bench "Check defined"   $ whnf Just         False
                                ],
  bgroup "justToJust (control)" [ bench "check Nothing  " $ whnf justToJust   Nothing
                                , bench "Check Just   "   $ whnf Just         False
                                ],
  bgroup "catchMethod1        " [ bench "Check undefined" $ whnf catchMethod1 undefined
                                , bench "Check defined"   $ whnf catchMethod1 False
                                ],
  bgroup "catchMethod2        " [ bench "Check undefined" $ whnf catchMethod2 undefined
                                , bench "Check defined"   $ whnf catchMethod2 False
                                ],
  bgroup "catchMethod3        " [ bench "Check undefined" $ whnf catchMethod3 undefined
                                , bench "Check defined"   $ whnf catchMethod3 False
                                ],
  bgroup "catchMethod4        " [ bench "Check undefined" $ whnf catchMethod4 undefined
                                , bench "Check defined"   $ whnf catchMethod4 False
                                ],
  bgroup "catchMethod5        " [ bench "Check undefined" $ whnf catchMethod5 undefined
                                , bench "Check defined"   $ whnf catchMethod5 False
                                ],
  bgroup "catchMethod6        " [ bench "Check undefined" $ whnf catchMethod6 undefined
                                , bench "Check defined"   $ whnf catchMethod6 False
                                ],
  bgroup "primspoon           " [ bench "Check undefined" $ whnf primspoon    undefined
                                , bench "Check defined"   $ whnf primspoon    False
                                ],
  bgroup "throwsMethod0       " [ bench "Check undefined" $ whnf throwsMethod0 undefined
                                , bench "Check defined"   $ whnf throwsMethod0 False
                                ],
  bgroup "throwsMethod1       " [ bench "Check undefined" $ whnf throwsMethod1 undefined
                                , bench "Check defined"   $ whnf throwsMethod1 False
                                ],
  bgroup "throwsMethod2       " [ bench "Check undefined" $ whnf throwsMethod2 undefined
                                , bench "Check defined"   $ whnf throwsMethod2 False
                                ],
  bgroup "throwsMethod3       " [ bench "Check undefined" $ whnf throwsMethod3 undefined
                                , bench "Check defined"   $ whnf throwsMethod3 False
                                ],
  bgroup "throwsMethod4       " [ bench "Check undefined" $ whnf throwsMethod4 undefined
                                , bench "Check defined"   $ whnf throwsMethod4 False
                                ],

  bgroup "throwsMethod5       " [ bench "Check undefined" $ whnf throwsMethod5 undefined
                                , bench "Check defined"   $ whnf throwsMethod5 False
                                ],
  bgroup "throws              " [ bench "Check undefined" $ whnf throws        undefined
                                , bench "Check defined"   $ whnf throws        False
                                ]]

