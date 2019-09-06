module GC.XorSpec where

import Data.Circuit
import Data.Circuit.GC.Evaluator ()
import Data.Circuit.GC.Garbler ()
import Data.Circuit.MVar ()
import Control.Monad.IO.Class
import Test.Hspec
import GC.Util

xor' :: RunCircuit m a => (a, a) -> m a
xor' (x, y) = do
  x' <- el $ Input x
  y' <- el $ Input y
  r <- xor2 x' y'
  return r

tests :: [((Bool, Bool), Maybe Bool)]
tests = [((False,False),Just False)
        ,((False,True ),Just True)
        ,((True ,False),Just True)
        ,((True ,True ),Just False)]

spec :: Spec
spec = describe "Xor test" $ sequence_ $ 
       Prelude.map (\(x,y) -> it (show x) $ do
                       r <- liftIO $ test xor' xor' x
                       r `shouldBe` y) $ concat $ replicate 10 tests

