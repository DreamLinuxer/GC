{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.GC.GarblerWriter where

import Data.Circuit
import Control.Monad.Writer.Lazy

type GTable = [((Bool,Bool),Bool)]

instance RunCircuit (Writer [GTable]) () where
  el (Wire x)  = return x
  el (Input x) = return x
  el (And2 c1 c2) = do
    _ <- el c1
    _ <- el c2
    let table = [[((False, False), False),
                  ((False,  True), False),
                  (( True, False), False),
                  (( True,  True),  True)]]
    tell table
    return ()
  el (Or2  c1 c2) = do
    _ <- el c1
    _ <- el c2
    let table = [[((False, False), False),
                  ((False,  True),  True),
                  (( True, False),  True),
                  (( True,  True),  True)]]
    tell table
    return ()
  el (Xor2 c1 c2) = do
    _ <- el c1
    _ <- el c2
    return ()

writer_add1_test :: [GTable]
writer_add1_test = execWriter $ add1 ((),(),())
