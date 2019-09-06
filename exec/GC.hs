{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Circuit
import Data.Circuit.GC.Evaluator ()
import Data.Circuit.GC.Garbler ()
import Data.Circuit.GC.Util
import Data.Circuit.Socket ()
import Data.Circuit.Util
import Data.Bits
import Data.ByteString hiding (putStrLn)
import Control.Monad.Reader
import Control.Exception ( bracket, catch )
import Control.Concurrent
import Control.Monad.State.Lazy
import Control.Concurrent.Async
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP
import Crypto.Random
import Test.Hspec

main :: IO ()
-- main = return ()
main = hspec $ do
  describe "add1 test" $ do
    it "" $ do
      r <- liftIO $ test (False,False,False)
      r `shouldBe` (Just (False,False))

test :: (Bool,Bool,Bool) -> (IO (Maybe (Bool,Bool)))
test input = do
  mv <- newEmptyMVar
  evaluator <- async $ evaluate mv
  garbler <- async $ garble input mv
  p <- wait garbler
  b <- wait evaluator
  let r = do
        (p1,p2) <- p
        (b1,b2) <- b
        return (p1 `xor` b1, p2 `xor` b2)
  -- putStrLn $ (show r)
  return r

garble :: (Bool, Bool, Bool) -> MVar () -> IO (Maybe (Bool, Bool))
garble (x1,x2,x3) mv = bracket
  ( socket :: IO (Socket Inet Stream TCP) )
  ( \s-> do
      close s
      putStrLn "Connecting socket closed."
  )
  ( \s-> do
      takeMVar mv
      setSocketOption s (ReuseAddress True)
      connect s (SocketAddressInet inetLoopback 1024)
      putStrLn "Connecting socket ready..."
      seed <- seedNew
      let drg = drgNewSeed seed
      let (delta, _)  = randomBytesGenerate labelSize drg
      ((b1,_), (b2,_)) <- runReaderT (evalStateT (add1' ((x1,empty),(x2,empty),(x3,empty)))
                                                 (delta :: ByteString, 0 :: Int)) s
      -- putStrLn $ "Garbler result = " ++ show (r1,r2)
      takeMVar mv
      return $ Just (b1,b2)
  )

evaluate :: MVar () -> IO (Maybe (Bool, Bool))
evaluate mv = bracket
  ( socket :: IO (Socket Inet Stream TCP) )
  ( \s-> do
    close s
    putStrLn "Listening socket closed."
  )
  ( \s-> do
    setSocketOption s (ReuseAddress True)
    bind s (SocketAddressInet inetLoopback 1024)
    listen s 10
    putStrLn "Listening socket ready..."
    putMVar mv ()
    acceptAndHandle s mv `catch` \e -> do print (e :: SocketException)
                                          return Nothing
  )

acceptAndHandle :: Socket Inet Stream TCP -> MVar () -> IO (Maybe (Bool, Bool))
acceptAndHandle s mv = bracket
  ( accept s )
  ( \(p, addr)-> do
    close p
    putStrLn $ "Closed connection to " ++ show addr
  )
  ( \(p, addr)-> do
      putStrLn $ "Accepted connection from " ++ show addr
      ((b1,_),(b2,_)) <- runReaderT (add1' $(replicateTuple 3 [| (True,empty) |])) p
      -- putStrLn $ "Evaluator result = " ++ show (r1,r2)
      putMVar mv ()
      return $ Just (b1,b2)
  )

