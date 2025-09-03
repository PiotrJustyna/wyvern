module Main where

import Content
import Data.Map
import Diagrams.Prelude
import ID
import SkewerBlock
import Test.HUnit

pinningTest1 :: Test
pinningTest1 =
  let dummyId1 = ID "1"
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (0.0, 0.0)) (Just dummyId1) empty []
   in TestCase (assertEqual "empty map of origins" 0 (length loopbackConnections))

pinningTest2 :: Test
pinningTest2 =
  let dummyId1 = ID "1"
      dummyId2 = ID "2"
      mapOfOrigins = singleton dummyId2 (p2 (0.1, 0.1))
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (0.0, 0.0)) (Just dummyId1) mapOfOrigins []
   in TestCase (assertEqual "map of origins does not contain the id" 0 (length loopbackConnections))

pinningTest3 :: Test
pinningTest3 =
  let dummyId1 = ID "1"
      mapOfOrigins = singleton dummyId1 (p2 (1.0, 1.0))
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (0.0, 0.0)) (Just dummyId1) mapOfOrigins []
   in TestCase
        ( do
            -- 2025-08-26 PJ:
            -- --------------
            -- limbo: this is a direct, 2-point connection
            -- should never happen - a diagonal line
            assertEqual "one loopback connection" 1 (length loopbackConnections)
            assertEqual "2-point limbo loopback connection" 2 (length $ head loopbackConnections)
            assertEqual "origin" (p2 (0.0, 0.0)) (head loopbackConnections !! 0)
            assertEqual "destination" (p2 (1.0, 1.0)) (head loopbackConnections !! 1)
        )

pinningTest4 :: Test
pinningTest4 =
  let dummyId1 = ID "1"
      mapOfOrigins = singleton dummyId1 (p2 (1.587, -2.1))
      existingLoopbacks = [[(p2 (1.0, 1.0)), (p2 (1.0, 1.0))]]
      (connection, loopbackConnections) =
        renderAdditionalConnection (p2 (2.7, -2.5)) (Just dummyId1) mapOfOrigins existingLoopbacks
   in TestCase
        ( do
            assertEqual "updated loopback connections" 2 (length loopbackConnections)
            assertEqual "4-point upward loopback connection" 4 (length $ head loopbackConnections)
            assertEqual "origin" (p2 (2.7, -2.5)) (head loopbackConnections !! 0)
            assertEqual "mid point 1" (p2 (4.2, -2.5)) (head loopbackConnections !! 1)
            assertEqual "mid point 2" (p2 (4.2, -2.2)) (head loopbackConnections !! 2)
            assertEqual "destination" (p2 (3.174, -2.2)) (head loopbackConnections !! 3)
            assertEqual "2-point dummy starting loopback connection" 2 (length $ (loopbackConnections !! 1))
            assertEqual "starting point" (p2 (1.0, 1.0)) ((loopbackConnections !! 1) !! 0)
        )

pinningTest5 :: Test
pinningTest5 =
  let dummyId1 = ID "1"
      mapOfOrigins = singleton dummyId1 (p2 (0.0, 1.0))
      existingLoopbacks = [[(p2 (1.0, 1.0)), (p2 (1.0, 1.0))]]
      origin = (p2 (3.0, -3.0))
      emptyBranch = ConnectedSkewerBlocks [] (Just dummyId1)
      fork = Fork dummyId1 origin (Content "dummy") emptyBranch emptyBranch
      (_, _, loopbackConnections0) = render' emptyBranch origin mapOfOrigins existingLoopbacks
      (_, loopbackConnections1) = SkewerBlock.render fork mapOfOrigins existingLoopbacks
      (_, loopbackConnections2) = SkewerBlock.render fork mapOfOrigins loopbackConnections1
   in TestCase
        ( do
            assertEqual "sanity test 0 for fork branch rendering" 2 (length loopbackConnections0)
            assertEqual "sanity test 1 for fork rendering" 3 (length loopbackConnections1)
            assertEqual "sanity test 2 for fork rendering" 5 (length loopbackConnections2)
        )

iconsRenderingOneFork :: Test
iconsRenderingOneFork =
  let dummyId1 = ID "1"
      mapOfOrigins = singleton dummyId1 (p2 (0.0, 1.0))
      origin = (p2 (3.0, -3.0))
      emptyBranch = ConnectedSkewerBlocks [] (Just dummyId1)
      fork = Fork dummyId1 origin (Content "dummy") emptyBranch emptyBranch
      (_, lastBlocksDepth, loopbackConnections) = renderIcons' [fork] mapOfOrigins
   in TestCase
        ( do
            assertEqual "icons rendering - one fork" 2 (length loopbackConnections)
        )

iconsRenderingManyForks :: Test
iconsRenderingManyForks =
  let dummyId1 = ID "1"
      mapOfOrigins = singleton dummyId1 (p2 (0.0, 1.0))
      origin = (p2 (3.0, -3.0))
      emptyBranch = ConnectedSkewerBlocks [] (Just dummyId1)
      fork = Fork dummyId1 origin (Content "dummy") emptyBranch emptyBranch
      (_, lastBlocksDepth, loopbackConnections) = renderIcons' [fork, fork, fork, fork, fork] mapOfOrigins
   in TestCase
        ( do
            assertEqual "icons rendering - many forks" 10 (length loopbackConnections)
        )

iconsRenderingMixedSkewerBlocks :: Test
iconsRenderingMixedSkewerBlocks =
  let dummyId1 = ID "1"
      dummyContent = (Content "dummy")
      action1Id = ID "1"
      fork1Id = ID "2"
      action1Origin = (p2 (2.0, -1.0))
      fork1Origin = (p2 (2.0, -2.0))
      mapOfOrigins = insert fork1Id fork1Origin (singleton action1Id action1Origin)
      emptyBranch = ConnectedSkewerBlocks [] Nothing
      action1 = Action action1Id action1Origin dummyContent
      action2 = Action (ID "#") (p2 (2.0, -3.0)) dummyContent
      action3 = Action (ID "#") (p2 (2.0, -5.0)) dummyContent
      fork1 =
        Fork
          fork1Id
          fork1Origin
          dummyContent
          (ConnectedSkewerBlocks [action2] Nothing)
          (ConnectedSkewerBlocks [] (Just (ID "2")))
      fork2 =
        Fork
          (ID "#")
          (p2 (2.0, -4.0))
          dummyContent
          (ConnectedSkewerBlocks [action3] Nothing)
          emptyBranch
      (_, lastBlocksDepth, loopbackConnections) = renderIcons' [action1, fork1, fork2] mapOfOrigins

      -- level 2:
      (_, loopbackConnections1) = SkewerBlock.render fork1 mapOfOrigins []
      (_, loopbackConnections2) = SkewerBlock.render fork2 mapOfOrigins loopbackConnections1

      -- level 3:
      (_, _, lLoopbackConnections) = render' (ConnectedSkewerBlocks [action3] Nothing) (p2 (2.0, -4.0)) mapOfOrigins loopbackConnections1
      (_, _, rLoopbackConnections) = render' emptyBranch (p2 (2.0, -4.0)) mapOfOrigins lLoopbackConnections
   in TestCase
        ( do
            -- level 1:
            assertEqual "icons rendering - mixed skewer blocks" 1 (length loopbackConnections)

            -- level 2:
            assertEqual "icons rendering - fork 1" 1 (length loopbackConnections1)
            assertEqual "icons rendering - fork 2" 1 (length loopbackConnections2)

            -- level 3:
            assertEqual "icons rendering - fork 2 left branch" 1 (length lLoopbackConnections)
            assertEqual "icons rendering - fork 2 right branch" 1 (length rLoopbackConnections)
        )

loopbackConflictTests' :: Test
loopbackConflictTests' =
  let x = 1.0
      y1' = 1.0
      y2' = 1.0
      emptyExistingLoopback = []
      existingLoopbackTooShort1 = [(p2 (1.0, 1.0))]
      existingLoopbackTooShort2 = [(p2 (1.0, 1.0)), (p2 (1.0, 1.0))]
      existingLoopbackTooShort3 = [(p2 (1.0, 1.0)), (p2 (1.0, 1.0)), (p2 (1.0, 1.0))]
      overlappingExistingLoopback1 = [(p2 (1.0, 1.0)), (p2 (1.0, 1.0)), (p2 (1.0, 0.0)), (p2 (1.0, 1.0))]
      overlappingExistingLoopback2 = [(p2 (1.0, 1.0)), (p2 (1.0, 0.0)), (p2 (1.0, 1.0)), (p2 (1.0, 1.0))]
      overlappingExistingLoopback3 = [(p2 (1.0, 1.0)), (p2 (1.0, 1.0)), (p2 (1.0, 1.0)), (p2 (1.0, 1.0))]
      overlappingExistingLoopback4 = [(p2 (1.0, 1.0)), (p2 (1.0, 0.0)), (p2 (1.0, 0.0)), (p2 (1.0, 1.0))]
      overlappingExistingLoopback5 = [(p2 (1.0, 1.0)), (p2 (1.0, y1' + 0.1)), (p2 (1.0, y2' - 0.1)), (p2 (1.0, 1.0))]
      nonoverlappingExistingLoopback1 = [(p2 (1.0, 1.0)), (p2 (1.0, 2.0)), (p2 (1.0, 2.0)), (p2 (1.0, 1.0))]
      nonoverlappingExistingLoopback2 = [(p2 (1.0, 1.0)), (p2 (x + 0.1, y1' + 0.1)), (p2 (x + 0.1, y2' - 0.1)), (p2 (1.0, 1.0))]
   in TestCase
        ( do
            assertEqual "loopback connection not present" False (loopbackConflict' emptyExistingLoopback x y1' y2')
            assertEqual "loopback connection too short 1" False (loopbackConflict' existingLoopbackTooShort1 x y1' y2')
            assertEqual "loopback connection too short 2" False (loopbackConflict' existingLoopbackTooShort2 x y1' y2')
            assertEqual "loopback connection too short 3" False (loopbackConflict' existingLoopbackTooShort3 x y1' y2')
            assertEqual "overlapping loopback connection 1" True (loopbackConflict' overlappingExistingLoopback1 x y1' y2')
            assertEqual "overlapping loopback connection 2" True (loopbackConflict' overlappingExistingLoopback2 x y1' y2')
            assertEqual "overlapping loopback connection 3" True (loopbackConflict' overlappingExistingLoopback3 x y1' y2')
            assertEqual "overlapping loopback connection 4" True (loopbackConflict' overlappingExistingLoopback4 x y1' y2')
            assertEqual "overlapping loopback connection 5" True (loopbackConflict' overlappingExistingLoopback5 x y1' y2')
            assertEqual "non-overlapping loopback connection 1" False (loopbackConflict' nonoverlappingExistingLoopback1 x y1' y2')
            assertEqual "non-overlapping loopback connection 2" False (loopbackConflict' nonoverlappingExistingLoopback2 x y1' y2')
        )

loopbackConflictTests :: Test
loopbackConflictTests =
  let x = 1.0
      conflictShift = 0.1
      y1' = 1.0
      y2' = 1.0
      emptyExistingLoopbacks = []
      overlappingExistingLoopback1 =
        [[(p2 (0.0, 0.0)), (p2 (x, 0.0)), (p2 (x, 1.0)), (p2 (0.0, 1.0))]]
      overlappingExistingLoopback2 =
        [ [(p2 (0.0, 0.0)), (p2 (x + conflictShift, 0.0)), (p2 (x + conflictShift, 1.0)), (p2 (0.0, 1.0))],
          [(p2 (0.0, 0.0)), (p2 (x, 0.0)), (p2 (x, 1.0)), (p2 (0.0, 1.0))]
        ]
   in TestCase
        ( do
            assertEqual "conflict not present" x (loopbackConflict emptyExistingLoopbacks x 1.0 1.0)
            assertEqual "overlapping loopback connection 1" (x + conflictShift) (loopbackConflict overlappingExistingLoopback1 x y1' y2')
            assertEqual "overlapping loopback connection 2" (x + conflictShift + conflictShift) (loopbackConflict overlappingExistingLoopback2 x y1' y2')
        )

sanityCheckTests :: Test
sanityCheckTests =
  TestList
    [ pinningTest1,
      pinningTest2,
      pinningTest3,
      pinningTest4,
      pinningTest5,
      iconsRenderingOneFork,
      iconsRenderingManyForks,
      iconsRenderingMixedSkewerBlocks,
      loopbackConflictTests,
      loopbackConflictTests'
    ]

main :: IO ()
main = runTestTTAndExit sanityCheckTests
