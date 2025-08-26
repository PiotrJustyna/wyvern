module Main where

import Data.Map
import Diagrams.Prelude
import ID
import SkewerBlock
import Test.HUnit

pinningTest1 :: Test
pinningTest1 =
  let dummyId1 = ID "1"
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (0.0, 0.0)) (Just dummyId1) empty
   in TestCase (assertEqual "empty map of origins" 0 (length loopbackConnections))

pinningTest2 :: Test
pinningTest2 =
  let dummyId1 = ID "1"
      dummyId2 = ID "2"
      mapOfOrigins = singleton dummyId2 (p2 (0.1, 0.1))
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (0.0, 0.0)) (Just dummyId1) mapOfOrigins
   in TestCase (assertEqual "map of origins does not contain the id" 0 (length loopbackConnections))

pinningTest3 :: Test
pinningTest3 =
  let dummyId1 = ID "1"
      mapOfOrigins = singleton dummyId1 (p2 (1.0, 1.0))
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (0.0, 0.0)) (Just dummyId1) mapOfOrigins
   in TestCase
        ( do
            -- 2025-08-26 PJ:
            -- --------------
            -- lilmbo: this is a direct, 2-point connection
            -- should never happen - a diagonal line
            assertEqual "2-point limbo loopback connection" 2 (length loopbackConnections)
            assertEqual "origin" (p2 (0.0, 0.0)) (loopbackConnections !! 0)
            assertEqual "destination" (p2 (1.0, 1.0)) (loopbackConnections !! 1)
        )

pinningTest4 :: Test
pinningTest4 =
  let dummyId1 = ID "1"
      mapOfOrigins = singleton dummyId1 (p2 (1.587, -2.1))
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (2.7, -2.5)) (Just dummyId1) mapOfOrigins
   in TestCase
        ( do
            assertEqual "4-point upward loopback connection" 4 (length loopbackConnections)
            assertEqual "origin" (p2 (2.7, -2.5)) (loopbackConnections !! 0)
            assertEqual "mid point 1" (p2 (4.3, -2.5)) (loopbackConnections !! 1)
            assertEqual "mid point 2" (p2 (4.3, -2.2)) (loopbackConnections !! 2)
            assertEqual "destination" (p2 (3.174, -2.2)) (loopbackConnections !! 3)
        )

sanityCheckTests :: Test
sanityCheckTests =
  TestList
    [ pinningTest1,
      pinningTest2,
      pinningTest3,
      pinningTest4
    ]

main :: IO ()
main = runTestTTAndExit sanityCheckTests
