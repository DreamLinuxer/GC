{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.BoolCircuit where

import Data.Circuit
import Data.Bits
import Control.Monad.Identity

instance RunCircuit Identity Bool where
  el (Input x) = return x
  el (Wire x)  = return x
  el (And2 c1 c2) = do
    b1 <- el c1
    b2 <- el c2
    return $ b1 && b2
  el (Or2  c1 c2) = do
    b1 <- el c1
    b2 <- el c2
    return $ b1 || b2
  el (Xor2 c1 c2) = do
    b1 <- el c1
    b2 <- el c2
    return $ xor b1 b2

bool_add1_test :: (Bool,Bool)
bool_add1_test = runIdentity $ add1 (True,True,True)
