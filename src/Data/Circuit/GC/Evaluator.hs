{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.GC.Evaluator where

import Data.Circuit
import Data.Circuit.GC.Util
import Data.ByteString as BS
import Data.ByteArray as BA
import Data.Array
import qualified Data.Bits as B
import Crypto.Hash

type Ty = (Bool, ByteString)

processBinGate :: (Monad m, ChannelMonad r m)
               => (Circuit Ty) -> (Circuit Ty) -> m Ty
processBinGate c1 c2 = do
  (p1', label1) <- el c1
  (p2', label2) <- el c2
  table <- receive
  let r = (table :: GTable) ! (fromIntegral $ indexB p1' p2')
  let h = hashWith SHA256 (BS.append label1 label2)
  let label3 = h `BA.xor` r
  return (getBit label3, clear label3)

instance (Monad m, ChannelMonad r m) => RunCircuit m Ty where
  el (Wire x)  = return x
  el (Input _) = do
    (p', label) <- receive
    return (p', label)
  el (And2 c1 c2) = processBinGate c1 c2
  el (Or2  c1 c2) = processBinGate c1 c2
  el (Xor2 c1 c2) = do
    (p1', label1) <- el c1
    (p2', label2) <- el c2
    return $ (p1' `B.xor` p2', label1 `xor` label2)
