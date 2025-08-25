module Main where

import ID
import SkewerBlock
import Test.HUnit
import Data.Map
import Diagrams.Prelude

sanityCheckTests :: Test
sanityCheckTests =
  let dummyId = ID "1"
      (connection, loopbackConnections) = renderAdditionalConnection (p2 (0.0, 0.0)) (Just dummyId) empty
  in  TestList [TestCase (assertEqual "just a test 1" dummyId dummyId),
                TestCase (assertEqual "just a test 2" dummyId dummyId),
                TestCase (assertEqual "just a test 3" (length loopbackConnections) 0)]

main :: IO ()
main = runTestTTAndExit sanityCheckTests
