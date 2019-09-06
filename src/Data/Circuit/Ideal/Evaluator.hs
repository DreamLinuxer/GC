{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.Ideal.Evaluator where

import Data.Circuit.Ideal.Circuit
import Data.ByteString

processBinGate :: (Monad m, ChannelMonad r m)
               => (Circuit (Maybe Bool) ByteString) -> (Circuit (Maybe Bool) ByteString)
               -> m ByteString
processBinGate c1 c2 = do
  label1 <- el c1
  label2 <- el c2
  send chEtoF (label1, label2)
  receive chFtoE

instance (Monad m, ChannelMonad r m) =>
         RunCircuit m (Maybe Bool) ByteString where
  el (Wire b) = return b
  el (Input b) = send chEtoF b >> receive chFtoE
  el (And2 c1 c2) = processBinGate c1 c2
  el (Or2  c1 c2) = processBinGate c1 c2
  el (Xor2 c1 c2) = processBinGate c1 c2
