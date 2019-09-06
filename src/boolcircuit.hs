{-# LANGUAGE MultiParamTypeClasses #-}

module BoolCircuit where

import Control.Monad

-- BoolCircuit represents all `computations` around boolean circuits.  Any of
-- such `computations` needs to define how binary AND, OR, and XOR gates should be
-- computed and (but implicitly) chained.
class Monad m => BoolCircuit m a where
  and2 :: a -> a -> m a
  or2  :: a -> a -> m a
  xor2 :: a -> a -> m a

  -- A general function to run any Boolean circuits
  run :: BoolCircuit m a => m a -> b

-- A 1-bit full-adder is an example circuit.
add1 :: BoolCircuit m a => (a, a, a) -> m (a,a)
add1 (x, y, cin) = do 
  t1 <- xor2 x cin
  t2 <- xor2 y cin
  s  <- xor2 x t2
  t3 <- and2 t1 t2
  cout <- xor2 cin t3
  return (s,cout)

-- Build an n-bit adder from a 1-bit adder
addn :: BoolCircuit m a => ([a], [a], a) -> m ([a], a)
addn ([], [], carryIn) = return ([], carryIn)
addn (a:as, b:bs, carryIn) = 
  do (sum, carry) <- add1 (a, b, carryIn)
     (sums, carryOut) <- addn (as, bs, carry)
     return (sum:sums, carryOut)
     
-- An one-bit multiplexer boolean circuit
mux1 :: BoolCircuit m a => (a, a, a) -> m a
mux1 (x, y, c) = do
  t <- xor2 x y
  t <- and2 t c
  ret <- xor2 t x
  return ret

-- Build an n-bit multiplexer from a 1-bit multiplexer
muxn :: BoolCircuit m a => ([a], [a], a) -> m [a]
muxn (a, b, c) = mapM mux1 $ zip3 a b (repeat c)
