{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.Ideal.MVar where

import Data.Circuit.Ideal.Circuit
import Data.Either.Combinators
import Data.ByteString
import Data.Serialize
import Control.Monad.Reader
import Control.Concurrent.MVar

instance (MonadIO m) =>
         ChannelMonad [MVar ByteString] (ReaderT [MVar ByteString] m) where
  send i x = do
    mv <- liftM (!! i) $ ask
    liftIO $ putMVar mv $ encode x
    return ()

  receive i = do
    mv <- liftM (!! i) $ ask
    p <- liftIO $ takeMVar mv
    return $ fromRight' . decode $ p

