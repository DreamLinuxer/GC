{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, InstanceSigs #-}

module Counter where

import Control.Monad
import Control.Monad.State.Lazy
import BoolCircuit

data GateCounters = Gcnts { nAND2 :: Integer,
                            nOR2 :: Integer,
                            nXOR2 :: Integer }
type Count = State GateCounters 
-- newtype Count a = Count { count :: State GateCounters a }

-- Count is an instance of BoolCircuit monad that counts and records the numbers
-- of and2's, or2's, and xor2's.
instance BoolCircuit Count (Maybe a) where
  and2 x y = do
    modify (\cnts@(Gcnts {nAND2 = n}) -> cnts {nAND2 = n+1})
    return Nothing
    
  or2  x y = do
    modify (\cnts@(Gcnts {nOR2 = n}) -> cnts {nOR2 = n+1})
    return Nothing
    
  xor2 x y = do
    modify (\cnts@(Gcnts {nXOR2 = n}) -> cnts {nXOR2 = n+1})
    return Nothing

  run :: Count (Maybe a) -> b
  run circuit = undefined -- execState circuit v
