module Main where

import LayoutTests
import PositionedBlockTests
import Test.Hspec
import ValidatorTests

layoutTests :: Spec
layoutTests = do
  specLayout1

positionedBlockTests :: Spec
positionedBlockTests = do
  specShow

validationTests :: Spec
validationTests = do
  specCategorizeId
  specFindIncorrectGammaConnections'
  specFindIncorrectGammaConnections''

allSpecs :: Spec
allSpecs = do
  validationTests
  positionedBlockTests
  layoutTests

main :: IO ()
main = hspec allSpecs
