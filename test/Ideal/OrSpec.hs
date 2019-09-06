module Ideal.OrSpec where

import Data.Circuit.Ideal.Circuit
import Data.Circuit.Ideal.Evaluator ()
import Data.Circuit.Ideal.Garbler ()
import Data.Circuit.Ideal.Fpre ()
import Data.Circuit.MVar ()
import Control.Monad.IO.Class
import Test.Hspec
import Ideal.Util

or' :: RunCircuit m a b => (a, a) -> m b
or' (x, y) = do
  x' <- el $ Input x
  y' <- el $ Input y
  r <- or2 x' y'
  return r

tests :: [((Maybe Bool, Maybe Bool), (Maybe Bool, Maybe Bool), Maybe Bool)]
tests = [((Just False,Just False),(Nothing,Nothing),Just False)
        ,((Just False,Just True ),(Nothing,Nothing),Just True )
        ,((Just True ,Just False),(Nothing,Nothing),Just True )
        ,((Just True ,Just True ),(Nothing,Nothing),Just True )
        ,((Just False,Nothing),(Nothing,Just False),Just False)
        ,((Just False,Nothing),(Nothing,Just True ),Just True )
        ,((Just True ,Nothing),(Nothing,Just False),Just True )
        ,((Just True ,Nothing),(Nothing,Just True ),Just True )
        ,((Nothing,Just False),(Just False,Nothing),Just False)
        ,((Nothing,Just True ),(Just False,Nothing),Just True )
        ,((Nothing,Just False),(Just True ,Nothing),Just True )
        ,((Nothing,Just True ),(Just True ,Nothing),Just True )
        ,((Nothing,Nothing),(Just False,Just False),Just False)
        ,((Nothing,Nothing),(Just False,Just True ),Just True )
        ,((Nothing,Nothing),(Just True ,Just False),Just True )
        ,((Nothing,Nothing),(Just True ,Just True ),Just True )]

spec :: Spec
spec = describe "And test" $ sequence_ $ 
       Prelude.map (\(gin,ein,x) -> it (show (gin,ein)) $ do
                       r <- liftIO $ test or' or' or' gin ein
                       r `shouldBe` x) $ concat $ replicate 10 tests
