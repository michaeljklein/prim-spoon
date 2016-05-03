{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

{-|
Module      : Control.Spoon.Prim
Description : This is the main module for prim-spoon
Copyright   : (c) Michael Klein, 2016
License     : GPL-3
Maintainer  : lambdamichael@gmail.com
Stability   : experimental

 This module exports `primspoon`. If you want the other
 spoon functions, you'll need "Control.Spoon".
-}

module Control.Spoon.Prim (primspoon, throws) where

import GHC.Prim (RealWorld, State#, catch#, realWorld#, seq#)
import Prelude (Bool(..), Maybe(..))

-- | This function takes an exception and a `State#` `RealWorld`, returning the output of `realWorld#` and `Nothing`.
exceptionToNothing :: a -> State# RealWorld -> (forall b. (# State# RealWorld, Maybe b #))
{-# INLINE exceptionToNothing #-}
exceptionToNothing _ _ = (# realWorld#, Nothing #)

-- | Evaluate a value to weak-head normal form and return Nothing if any exceptions are thrown during evaluation. For any error-free value, @primspoon = Just@.
primspoon :: a -> Maybe a
{-# INLINE primspoon #-}
primspoon x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, y #) -> (# t, Just y #)) (f s)) (seq# x)) exceptionToNothing realWorld#)

-- | Check whether a value, when evalueated to weak-head normal form, throws an error/exception. Note that this will evaluate the value to weak-head normal form.
throws :: a -> Bool
{-# INLINE throws #-}
throws x = (\(# _, v #) -> v) (catch# ((\f s -> (\(# t, _ #) -> (# t, False #)) (f s)) (seq# x)) (\_ u -> (# u, True #)) realWorld#)

