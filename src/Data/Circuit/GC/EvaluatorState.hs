{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.GC.EvaluatorState where

import Data.Circuit
import Data.Circuit.GC.GarblerWriter
import Data.Bits
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:_) = Just x

processBinGate :: (Circuit Bool) -> (Circuit Bool) -> MaybeT (State [GTable]) Bool
processBinGate c1 c2 = do
  b1 <- el c1
  b2 <- el c2
  table <- MaybeT $ gets head'
  modify tail
  let r = lookup (b1,b2) table
  guard $ isJust r
  return $ fromJust r

instance RunCircuit (MaybeT (State [GTable])) Bool where
  el (Input x) = return x
  el (Wire x)  = return x
  el (And2 c1 c2) = processBinGate c1 c2
  el (Or2  c1 c2) = processBinGate c1 c2
  el (Xor2 c1 c2) = do
    b1 <- el c1
    b2 <- el c2
    return $ xor b1 b2

state_add1_test :: Maybe (Bool,Bool)
state_add1_test = evalState (runMaybeT (add1 (True,True,True))) writer_add1_test
