module Ideal.Util where

import Data.Circuit.Ideal.Circuit ()
import Data.Circuit.Ideal.Fpre ()
import Data.Circuit.Ideal.Evaluator ()
import Data.Circuit.Ideal.Garbler ()
import Data.Circuit.GC.Util
import Data.Circuit.Ideal.MVar ()
import Data.ByteArray
import Data.ByteString hiding (putStrLn)
import Control.Monad.Reader
import Control.Concurrent
import Control.Monad.State.Lazy
import Control.Concurrent.Async
import Crypto.Random

test :: ((Maybe Bool, Maybe Bool) ->
         (ReaderT ByteString (ReaderT [MVar ByteString] IO) ()))
     -> ((Maybe Bool, Maybe Bool) ->
         ReaderT [MVar ByteString] IO ByteString)
     -> (((),()) ->
         StateT Int (ReaderT [MVar ByteString] IO) ByteString)
     -> (Maybe Bool,Maybe Bool)
     -> (Maybe Bool,Maybe Bool)
     -> (IO (Maybe Bool))
test circuitG circuitE circuitF inputG inputE = do
  mvs <- forM [1..3 :: Int] (const newEmptyMVar)
  drg <- getSystemDRG
  let (delta, _)  = randomBytesGenerate labelSize drg

  fPre <- async $ fpre circuitF mvs
  garbler <- async $ garble circuitG inputG delta mvs
  evaluator <- async $ evaluate circuitE inputE mvs

  wait garbler
  label <- wait fPre
  result <- wait evaluator
  putStrLn $ show (label,label `xor` delta :: ByteString,result)
  
  return $ dec label delta result
  where
    dec label delta label'
      | label' == label               = Just False
      | label' == (label `xor` delta) = Just True
      | otherwise                     = Nothing

garble :: ((Maybe Bool, Maybe Bool) ->
           (ReaderT ByteString (ReaderT [MVar ByteString] IO) ()))
       -> (Maybe Bool, Maybe Bool)
       -> ByteString -> [MVar ByteString] -> IO ()
garble circuit (x1,x2) delta mvs = do
  runReaderT (runReaderT (circuit (x1,x2)) delta) mvs

evaluate :: ((Maybe Bool, Maybe Bool) ->
             ReaderT [MVar ByteString] IO ByteString)
         -> (Maybe Bool, Maybe Bool) -> [MVar ByteString]
         -> IO ByteString
evaluate circuit (x1,x2) mvs = runReaderT (circuit (x1,x2)) mvs

fpre :: (((),()) ->
         StateT Int (ReaderT [MVar ByteString] IO) ByteString)
     -> [MVar ByteString] -> IO ByteString
fpre circuit mvs = runReaderT (evalStateT (circuit ((),())) (0 :: Int)) mvs
