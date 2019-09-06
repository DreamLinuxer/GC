{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.Ideal.Fpre where

import Data.Circuit.Ideal.Circuit
import Data.Circuit.GC.Util
import Data.ByteString
import Data.ByteArray
import Control.Monad.State.Lazy

enc :: Bool -> ByteString -> ByteString -> ByteString
enc b label delta = if b then label `xor` delta else label

processBinGate :: (Monad m, ChannelMonad r m)
               => (Circuit () ByteString) -> (Circuit () ByteString)
               -> (Bool -> Bool -> Bool)
               -> (StateT Int m) ByteString
processBinGate c1 c2 f = do
  label1 <- el c1
  label2 <- el c2
  delta <- receive chGtoF
  label3 <- liftM (randByteString delta) $ get
  (label1', label2') <- receive chEtoF
  let (b1,b2) = ((getBool label1 delta label1'), (getBool label2 delta label2'))
  modify (+1)
  send chFtoE $ enc (f b1 b2) label3 delta
  return label3
  where
    getBool false_label delta label
      | label == false_label             = False
      | label == false_label `xor` delta = True
      | otherwise                        = error "Wrong label"

instance (Monad m, ChannelMonad r m) =>
         RunCircuit (StateT Int m) () ByteString where
  el (Wire x) = return x
  el (Input _) = do
    (b1, delta) <- receive chGtoF
    b2 <- receive chEtoF
    label <- liftM (randByteString delta) $ get
    modify (+1)
    send chFtoE $ enc (getBool b1 b2) label delta
    return label
    where
      getBool (Just b) Nothing  = b
      getBool Nothing  (Just b) = b
      getBool _        _        = error "Input Error"
  el (And2 c1 c2) = processBinGate c1 c2 (&&)
  el (Or2  c1 c2) = processBinGate c1 c2 (||)
  el (Xor2 c1 c2) = do
    label1 <- el c1
    label2 <- el c2
    (label1', label2') <- receive chEtoF
    let label3 = label1 `xor` label2
    modify (+1)
    send chFtoE $ ((label1' :: ByteString) `xor` (label2' :: ByteString) :: ByteString)
    return label3
