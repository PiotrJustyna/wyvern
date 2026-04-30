module Layout2Tests where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import ID
import Layout
import PositionedBlock
import Test.Hspec

-- | Helper function to create test blocks for layout positioning
createTestBlocks :: [Block]
createTestBlocks =
  let f2LeftBranch = [Action Nothing "A4"]
      f2RightBranch = [Action Nothing "A5", Action Nothing "A6"]
      f1LeftBranch = [Action Nothing "A2"]
      f1RightBranch = [Action Nothing "A3", Fork Nothing "F2" f2LeftBranch f2RightBranch Nothing]
   in [Fork Nothing "F1" f1LeftBranch f1RightBranch Nothing, Action Nothing "A7"]

specLayout2 :: Spec
specLayout2 = describe "layout2" $ do
  let ([_a, (PositionedFork _i _c [l] r _gCId x y forkMaxX formMinY)], maxX, minY) = position' createTestBlocks 0.0 0.0

  context "Fork block positioning" $ do
    context "Fork root node" $ do
      it "should position at x=0.0" $
        x `shouldBe` 0.0
      it "should position at y=0.0" $
        y `shouldBe` 0.0

    context "Fork left branch" $ do
      let (lx, ly, lMaxX, lMinY) = getPosition l
      it "should position left branch at x=0.0" $
        lx `shouldBe` 0.0
      it "should position left branch one level below fork (y=-1*boxHeight)" $
        ly `shouldBe` (defaultBoundingBoxHeight * (-1.0))
      it "should calculate correct maximum x coordinate" $
        lMaxX `shouldBe` defaultBoundingBoxWidth
      it "should calculate correct maximum y coordinate" $
        lMinY `shouldBe` (defaultBoundingBoxHeight * (-2.0))

    context "Fork right branch" $ do
      let [(PositionedAction _i _c ax ay aMaxX aMinY), _f] = r
      it "should correctly position right branch - x coordinate" $
        ax `shouldBe` defaultBoundingBoxWidth
      it "should correctly position right branch - y coordinate" $
        ay `shouldBe` defaultBoundingBoxHeight * (-1.0)
      it "should calculate correct maximum x coordinate" $
        aMaxX `shouldBe` defaultBoundingBoxWidth * (2.0)
      it "should calculate correct maximum y coordinate" $
        aMinY `shouldBe` (defaultBoundingBoxHeight * (-2.0))

    context "Overall layout boundaries" $ do
      it "should calculate correct maximum x coordinate" $
        maxX `shouldBe` 9.0
      it "should calculate correct minimum y coordinate (deepest stack depth)" $
        minY `shouldBe` (defaultBoundingBoxHeight * (-6.0))
