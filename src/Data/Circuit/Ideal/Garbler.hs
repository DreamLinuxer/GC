{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.Ideal.Garbler where

import Data.Circuit.Ideal.Circuit
import Data.ByteString
import Control.Monad.Trans
import Control.Monad.Trans.Reader

processBinGate :: (Monad m, ChannelMonad r m)
               => (Circuit (Maybe Bool) ()) -> (Circuit (Maybe Bool) ())
               -> ReaderT ByteString m ()
processBinGate c1 c2 = el c1 >> el c2 >> ask >>= lift . send chGtoF

instance (Monad m, ChannelMonad r m) =>
         RunCircuit (ReaderT ByteString m) (Maybe Bool) () where
  el (Wire b) = return b
  el (Input b) = ask >>= lift . send chGtoF . (,) b
  el (And2 c1 c2) = processBinGate c1 c2
  el (Or2  c1 c2) = processBinGate c1 c2
  el (Xor2 c1 c2) = el c1 >> el c2
