module Main where

import Test.Hspec
import ValidatorTests (specCategorizeId, specFindIncorrectGammaConnections', specFindIncorrectGammaConnections'')

allSpecs :: Spec
allSpecs = do
  specCategorizeId
  specFindIncorrectGammaConnections'
  specFindIncorrectGammaConnections''

main :: IO ()
main = hspec allSpecs
