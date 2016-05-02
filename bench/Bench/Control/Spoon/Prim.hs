{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Bench.Control.Spoon.Prim (benchControlSpoonPrim) where


import Criterion.Main
import Control.Spoon      (teaspoon)
import GHC.Prim           (catch#, realWorld#, seq#)
import Control.Spoon.Prim

-- | Control mehod for benchmarking (appx. lower bound for a functional solution)
justToJust :: Maybe a -> Maybe (Maybe a)
justToJust y@(Just _) = Just y
justToJust Nothing    = Nothing

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
  bgroup "primspoon           " [ bench "Check undefined" $ whnf primspoon    undefined
                                , bench "Check defined"   $ whnf primspoon    False
                                ]]

