{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.MVar where

import Data.Circuit
import Data.Either.Combinators
import Data.ByteString
import Data.Serialize
import Control.Monad.Reader
import Control.Concurrent.MVar

instance (MonadIO m) =>
         ChannelMonad (MVar ByteString)
                      (ReaderT (MVar ByteString) m) where
  send x = do
    mv <- ask
    _ <- liftIO $ putMVar mv $ encode x
    return ()

  receive = do
    mv <- ask
    p <- liftIO $ takeMVar mv
    return $ fromRight' . decode $ p
