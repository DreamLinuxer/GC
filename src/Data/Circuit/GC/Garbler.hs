{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.GC.Garbler () where

import Data.Circuit
import Data.Circuit.GC.Util
import Data.ByteArray as BA
import Data.ByteString as BS
import Data.Bits as B
import Control.Monad.State.Lazy

type Ty = (Bool, ByteString)
type STy = (ByteString, Int)

getRandomBytes :: (Monad m) => StateT STy m ByteString
getRandomBytes = do
  (delta, n) <- get
  let r = randByteString delta n
  put (delta, n+1)
  return r

generate :: (Monad m) => StateT STy m (ByteString, ByteString, Bool)
generate = do
  delta <- liftM fst $ get
  label <- liftM (BA.take labelSize) $ getRandomBytes
  let p = testBit (BS.head label) 0
  return (delta, clear label, p)

processBinGate :: (Monad m, ChannelMonad r m)
               => (Circuit Ty) -> (Circuit Ty) -> Table -> StateT STy m Ty
processBinGate c1 c2 t = do
    (delta, label3, p3) <- generate
    (p1, label1) <- el c1
    (p2, label2) <- el c2
    let table = encrypt delta label1 label2 label3
              $ permute p1 p2 p3 t
    send table
    return (p3, label3)

instance (Monad m, ChannelMonad r m) =>
         RunCircuit (StateT STy m) Ty where
  el (Wire x) = return x
  el (Input (b,_)) = do
    (delta, label, p) <- generate
    let p' = p `B.xor` b
    if p' then send $ (p', (label `BA.xor` delta :: ByteString))
          else send $ (p', label)
    return (p, label)
  el (And2 c1 c2) = processBinGate c1 c2 and_table
  el (Or2  c1 c2) = processBinGate c1 c2 or_table
  el (Xor2 c1 c2) = do
    (p1, label1) <- el c1
    (p2, label2) <- el c2
    return (p1 `B.xor` p2, label1 `BA.xor` label2)
