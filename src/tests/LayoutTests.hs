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
    let ([positionedBlock1, positionedBlock2, (PositionedFork _i _c [l3] [r3] _gCId x3 y3)], maxX, minY) = position' blocks 0.0 0.0
    let (x1, y1) = getPosition positionedBlock1
    let (x2, y2) = getPosition positionedBlock2
    let (x3l, y3l) = getPosition l3
    let (x3r, y3r) = getPosition r3

    x1 `shouldBe` 0.0
    y1 `shouldBe` (defaultBoundingBoxHeight * (-3.0))

    x2 `shouldBe` 0.0
    y2 `shouldBe` defaultBoundingBoxHeight * (-2.0)

    x3 `shouldBe` 0.0
    y3 `shouldBe` 0.0
    x3l `shouldBe` 0.0
    y3l `shouldBe` (defaultBoundingBoxHeight * (-1.0))
    x3r `shouldBe` 0.0 + defaultBoundingBoxWidth
    y3r `shouldBe` (defaultBoundingBoxHeight * (-1.0))

    maxX `shouldBe` 6.0
    minY `shouldBe` (defaultBoundingBoxHeight * (-4.0))
