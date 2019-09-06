module GC.AndSpec where

import Data.Circuit
import Data.Circuit.GC.Evaluator ()
import Data.Circuit.GC.Garbler ()
import Data.Circuit.MVar ()
import Control.Monad.IO.Class
import Test.Hspec
import GC.Util

and' :: RunCircuit m a => (a, a) -> m a
and' (x, y) = do
  x' <- el $ Input x
  y' <- el $ Input y
  r <- and2 x' y'
  return r

tests :: [((Bool, Bool), Maybe Bool)]
tests = [((False,False),Just False)
        ,((False,True ),Just False)
        ,((True ,False),Just False)
        ,((True ,True ),Just True)]

spec :: Spec
spec = describe "And test" $ sequence_ $ 
       Prelude.map (\(x,y) -> it (show x) $ do
                       r <- liftIO $ test and' and' x
                       r `shouldBe` y) $ concat $ replicate 10 tests
