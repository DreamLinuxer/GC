{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Circuit where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Serialize

data Circuit a where
  Input :: a -> Circuit a
  Wire  :: a -> Circuit a
  And2  :: Circuit a -> Circuit a -> Circuit a
  Or2   :: Circuit a -> Circuit a -> Circuit a
  Xor2  :: Circuit a -> Circuit a -> Circuit a

class (MonadReader r m) => ChannelMonad r m where
  send :: Serialize a => a -> m ()
  receive :: Serialize a => m a

instance (ChannelMonad r m) => ChannelMonad r (StateT t m) where
  send = lift . send
  receive = lift receive

class (Monad m) => RunCircuit m a where
  el :: Circuit a -> m a

and2, or2, xor2 :: RunCircuit m a => a -> a -> m a
and2 c1 c2 = el $ And2 (Wire c1) (Wire c2)
or2  c1 c2 = el $ Or2  (Wire c1) (Wire c2)
xor2 c1 c2 = el $ Xor2 (Wire c1) (Wire c2)

add1 :: RunCircuit m a => (a, a, a) -> m (a,a)
add1 (x, y, cin) = do
  t1 <- xor2 x cin
  t2 <- xor2 y cin
  s  <- xor2 x t2
  t3 <- and2 t1 t2
  cout <- xor2 cin t3
  return (s,cout)

add1' :: RunCircuit m a => (a, a, a) -> m (a,a)
add1' (x, y, cin) = do
  x' <- el $ Input x
  y' <- el $ Input y
  cin' <- el $ Input cin
  r <- add1 (x',y',cin')
  return r
