module GC.Util where

import Data.Circuit.GC.Evaluator ()
import Data.Circuit.GC.Garbler ()
import Data.Circuit.GC.Util
import Data.Circuit.Socket ()
import Data.Bits
import Data.ByteString hiding (putStrLn)
import Control.Monad.Reader
import Control.Concurrent
import Control.Monad.State.Lazy
import Control.Concurrent.Async
import Crypto.Random

test :: (((Bool,ByteString),(Bool,ByteString))
        -> StateT (ByteString, Int) (ReaderT (MVar ByteString) IO) (Bool,ByteString))
     -> (((Bool, ByteString), (Bool, ByteString))
        -> (ReaderT (MVar ByteString) IO) (Bool,ByteString))
     -> (Bool,Bool) -> (IO (Maybe Bool))
test circuit circuit' input = do
  mv <- newEmptyMVar
  evaluator <- async $ evaluate circuit' mv
  garbler <- async $ garble circuit input mv
  p <- wait garbler
  b <- wait evaluator
  let r = do
        p' <- p
        b' <- b
        return (p' `xor` b')
  return r

garble :: (((Bool,ByteString),(Bool,ByteString))
          -> StateT (ByteString, Int) (ReaderT (MVar ByteString) IO) (Bool,ByteString))
       -> (Bool, Bool) -> MVar ByteString -> IO (Maybe Bool)
garble circuit (x1,x2) mv = do
  drg <- getSystemDRG
  let (delta, _)  = randomBytesGenerate labelSize drg
  (b,_) <- runReaderT (evalStateT (circuit ((x1,empty),(x2,empty)))
                                  (clear delta, 0 :: Int)) mv
  return $ Just b

evaluate :: (((Bool, ByteString), (Bool, ByteString))
            -> (ReaderT (MVar ByteString) IO) (Bool,ByteString))
         ->  MVar ByteString -> IO (Maybe Bool)
evaluate circuit mv = do
  (b,_) <- runReaderT (circuit ((False,empty),(False,empty))) mv
  return $ Just b
