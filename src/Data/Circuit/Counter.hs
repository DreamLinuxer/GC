{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.Counter where

import Control.Monad.Writer
import Data.Circuit

instance RunCircuit (Writer (Sum Int)) a where
  el :: Circuit a -> Writer (Sum Int) a
  el (Input x) = return x
  el (Wire x)  = return x
  el (And2 c1 c2) = do
    tell 1
    _ <- el c1
    r <- el c2
    return r
  el (Or2  c1 c2) = do
    tell 1
    _ <- el c1
    r <- el c2
    return r
  el (Xor2 c1 c2) = do
    tell 1
    _ <- el c1
    r <- el c2
    return r

count_add1_test :: Int
count_add1_test = getSum $ execWriter $ add1 (1,2,3 :: Int)
