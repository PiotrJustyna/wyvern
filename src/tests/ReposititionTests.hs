module ReposititionTests where

import Blocks
import Constants
import ID
import Layout
import PositionedBlock
import Test.Hspec

specReposition :: Spec
specReposition = describe "reposition" $ do
  context "PositionedBlock" $ do
    it "is up from the coordinates - does not get repositioned" $ do
      let x = 10.0
      let y = -10.0
      let irrelevantNumber = 0.0
      let maxx = irrelevantNumber
      let miny = irrelevantNumber
      let positionedAction = PositionedAction Nothing "" x y maxx miny
      let (repositionedAction, isRepositioned) = reposition'' positionedAction (y - 1.0)
      let repositionedActionPosition@(x', y', maxx', miny') = getPosition repositionedAction
      x `shouldBe` x'
      y `shouldBe` y'
      maxx `shouldBe` maxx'
      miny `shouldBe` miny'
      isRepositioned `shouldBe` False

    it "is down from the coordinates - gets repositioned" $ do
      let x = 10.0
      let y = -10.0
      let irrelevantNumber = 0.0
      let maxx = irrelevantNumber
      let miny = irrelevantNumber
      let positionedAction = PositionedAction Nothing "" x y maxx miny
      let (repositionedAction, isRepositioned) = reposition'' positionedAction (y + 1.0)
      let repositionedActionPosition@(x', y', maxx', miny') = getPosition repositionedAction
      x `shouldBe` x'
      (y - repositionShift) `shouldBe` y'
      maxx `shouldBe` maxx'
      (miny - repositionShift) `shouldBe` miny'
      isRepositioned `shouldBe` True
