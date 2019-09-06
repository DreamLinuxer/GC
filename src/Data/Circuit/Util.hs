{-# LANGUAGE TemplateHaskell #-}

module Data.Circuit.Util where

import Language.Haskell.TH

replicateTuple :: Int -> ExpQ -> Q Exp
replicateTuple n eq = do
  e <- eq
  return $ TupE $ replicate n e
