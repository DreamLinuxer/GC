module GC.OrSpec where

import Data.Circuit
import Data.Circuit.GC.Evaluator ()
import Data.Circuit.GC.Garbler ()
import Data.Circuit.MVar ()
import Control.Monad.IO.Class
import Test.Hspec
import GC.Util

or' :: RunCircuit m a => (a, a) -> m a
or' (x, y) = do
  x' <- el $ Input x
  y' <- el $ Input y
  r <- or2 x' y'
  return r

tests :: [((Bool, Bool), Maybe Bool)]
tests = [((False,False),Just False)
        ,((False,True ),Just True)
        ,((True ,False),Just True)
        ,((True ,True ),Just True)]

spec :: Spec
spec = describe "Or test" $ sequence_ $ 
       Prelude.map (\(x,y) -> it (show x) $ do
                       r <- liftIO $ test or' or' x
                       r `shouldBe` y) $ concat $ replicate 10 tests

