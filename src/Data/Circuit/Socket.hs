{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Circuit.Socket where

import Data.Circuit
import Data.Int
import Data.Either.Combinators
import qualified Data.ByteString as BS
import Data.Serialize
import Control.Monad.Reader
import System.Socket as S
import System.Socket.Family.Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

instance (MonadIO m) =>
         ChannelMonad (Socket Inet Stream TCP)
                      (ReaderT (Socket Inet Stream TCP) m) where
  send x = do
    s <- ask
    let p = encode x
    _ <- liftIO $ S.send s (encode ((fromIntegral $ BS.length p) :: Int32)) msgNoSignal
    -- liftIO $ putStrLn $ "send " ++ show ((fromIntegral $ BS.length p) :: Int32)
    _ <- liftIO $ S.send s p msgNoSignal
    -- liftIO $ putStrLn $ "send data"
    return ()

  receive = do
    s <- ask
    len <- liftIO $ ( liftM (fromRight' . decode)
                  $ S.receive s 4 msgNoSignal)
    -- liftIO $ putStrLn $ "receive " ++ show (len :: Int32)
    p <- liftIO $ S.receive s (fromIntegral (len :: Int32)) msgNoSignal
    -- liftIO $ putStrLn $ "receive data"
    return $ fromRight' . decode $ p
