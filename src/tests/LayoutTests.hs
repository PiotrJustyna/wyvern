module LayoutTests where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import ID
import Layout
import PositionedBlock
import Test.Hspec

specLayout1 :: Spec
specLayout1 = describe "layout1" $ do
  it "positions blocks simple in a simple linear layout" $ do
    let block1 = Action Nothing "-"
    let block2 = Action (Just (ID "1")) "1"
    let block3 = Fork Nothing "-" [Action (Just (ID "2")) "2"] [Action (Just (ID "3")) "3"] Nothing
    let blocks = [block1, block2, block3]
    let [(PositionedFork _i _c [l1] [r1] _gCId x1 y1), positionedBlock2, positionedBlock3] = position blocks 0.0 0.0
    let (x2, y2) = getPosition positionedBlock2
    let (x3, y3) = getPosition positionedBlock3
    let (x1l, y1l) = getPosition l1
    let (x1r, y1r) = getPosition r1

    x1 `shouldBe` 0.0
    y1 `shouldBe` (defaultBoundingBoxHeight * (-2.0))
    x1l `shouldBe` 0.0
    y1l `shouldBe` (defaultBoundingBoxHeight * (-3.0))
    x1r `shouldBe` 0.0 + defaultBoundingBoxWidth
    y1r `shouldBe` (defaultBoundingBoxHeight * (-3.0))

    x2 `shouldBe` 0.0
    y2 `shouldBe` (0.0 - defaultBoundingBoxHeight)

    x3 `shouldBe` 0.0
    y3 `shouldBe` 0.0
