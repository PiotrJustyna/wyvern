module LayoutTests where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import ID
import Layout
import PositionedBlock
import Test.Hspec

-- | Helper function to create test blocks for layout positioning
createTestBlocks :: [Block]
createTestBlocks =
  [ Action Nothing "-",
    Action (Just (ID "1")) "1",
    Fork Nothing "-" [Action (Just (ID "2")) "2"] [Action (Just (ID "3")) "3"] Nothing
  ]

-- | Helper function to position blocks at origin (0.0, 0.0)
positionTestBlocks :: ([PositionedBlock], Double, Double)
positionTestBlocks = position' createTestBlocks 0.0 0.0

-- | Helper function to extract positioned blocks and boundaries from layout result
-- Returns: (first block, second block, left fork child, right fork child, fork position, max x, min y)
extractPositions ::
  ([PositionedBlock], Double, Double) ->
  (PositionedBlock, PositionedBlock, PositionedBlock, PositionedBlock, (Double, Double), Double, Double)
extractPositions (positionedBlocks, skewerMaxX, skewerMinY) =
  let [pb1, pb2, (PositionedFork _i _c [l3] [r3] _gCId x3 y3 _maxX _minY)] = positionedBlocks
   in (pb1, pb2, l3, r3, (x3, y3), skewerMaxX, skewerMinY)

-- | Helper function to get position coordinates (x, y, maxX, minY) from a positioned block
getPositionCoords :: PositionedBlock -> (Double, Double, Double, Double)
getPositionCoords = getPosition

specLayout1 :: Spec
specLayout1 = describe "layout1" $ do
  let (pb1, pb2, l3, r3, (forkX, forkY), maxX, minY) = extractPositions positionTestBlocks

  context "First action block positioning" $ do
    let (x1, y1, _, _) = getPositionCoords pb1
    it "should position at x=0.0" $
      x1 `shouldBe` 0.0
    it "should position at y=-3*boxHeight (three blocks stacked below)" $
      y1 `shouldBe` (defaultBoundingBoxHeight * (-3.0))

  context "Second action block positioning" $ do
    let (x2, y2, _, _) = getPositionCoords pb2
    it "should position at x=0.0" $
      x2 `shouldBe` 0.0
    it "should position at y=-2*boxHeight (two blocks above it)" $
      y2 `shouldBe` (defaultBoundingBoxHeight * (-2.0))

  context "Fork block positioning" $ do
    context "Fork root node" $ do
      it "should position fork root at x=0.0" $
        forkX `shouldBe` 0.0
      it "should position fork root at y=0.0" $
        forkY `shouldBe` 0.0

    context "Fork left branch" $ do
      let (x3l, y3l, _, _) = getPositionCoords l3
      it "should position left branch at x=0.0" $
        x3l `shouldBe` 0.0
      it "should position left branch one level below fork (y=-1*boxHeight)" $
        y3l `shouldBe` (defaultBoundingBoxHeight * (-1.0))

    context "Fork right branch" $ do
      let (x3r, y3r, _, _) = getPositionCoords r3
      it "should position right branch at x=boxWidth (offset for fork branches)" $
        x3r `shouldBe` (0.0 + defaultBoundingBoxWidth)
      it "should position right branch at same level as left branch (y=-1*boxHeight)" $
        y3r `shouldBe` (defaultBoundingBoxHeight * (-1.0))

  context "Overall layout boundaries" $ do
    it "should calculate correct maximum x coordinate" $
      maxX `shouldBe` 6.0
    it "should calculate correct minimum y coordinate (deepest stack depth)" $
      minY `shouldBe` (defaultBoundingBoxHeight * (-4.0))
