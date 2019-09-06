{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Circuit.Ideal.Circuit where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Serialize

data Circuit a b where
  Input :: a -> Circuit a b
  Wire  :: b -> Circuit a b
  And2  :: Circuit a b -> Circuit a b -> Circuit a b
  Or2   :: Circuit a b -> Circuit a b -> Circuit a b
  Xor2  :: Circuit a b -> Circuit a b -> Circuit a b

class (MonadReader r m) => ChannelMonad r m where
  send :: Serialize a => Int -> a -> m ()
  receive :: Serialize a => Int -> m a

instance (ChannelMonad r m) => ChannelMonad r (StateT t m) where
  send n = lift . (send n)
  receive = lift . receive

class (Monad m) => RunCircuit m a b | m b -> a where
  el :: Circuit a b -> m b

and2, or2, xor2 :: RunCircuit m a b => b -> b -> m b
and2 c1 c2 = el $ And2 (Wire c1) (Wire c2)
or2  c1 c2 = el $ Or2  (Wire c1) (Wire c2)
xor2 c1 c2 = el $ Xor2 (Wire c1) (Wire c2)

chGtoF, chEtoF, chFtoE :: Int
chGtoF = 0
chEtoF = 1
chFtoE = 2
