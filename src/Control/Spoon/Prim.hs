{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

module Control.Spoon.Prim where

import GHC.Prim (RealWorld, State#, catch#, realWorld#, seq#)
import Prelude (Maybe(..))

exceptionToNothing :: a -> State# RealWorld -> (forall b. (# State# RealWorld, Maybe b #))
{-# INLINE exceptionToNothing #-}
exceptionToNothing _ _ = (# realWorld#, Nothing #)

-- | Evaluate a value to weak-head normal form and return Nothing if any exceptions are thrown during evaluation. For any error-free value, @primspoon = Just@.
primspoon :: a -> Maybe a
{-# INLINE primspoon #-}
primspoon x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, y #) -> (# t, Just y #)) (f s)) (seq# x)) exceptionToNothing realWorld#)
