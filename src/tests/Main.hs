module Main where

import Layout1Tests
import Layout2Tests
import Layout3Tests
import PositionedBlockTests
import ReposititionTests
import Test.Hspec
import ValidatorTests

repositionTests :: Spec
repositionTests = do
  specReposition

layoutTests :: Spec
layoutTests = do
  specLayout1
  specLayout2
  specLayout3

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
  repositionTests

main :: IO ()
main = hspec allSpecs
