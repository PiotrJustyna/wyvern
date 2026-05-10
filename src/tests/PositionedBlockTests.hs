module PositionedBlockTests where

import Blocks
import ID
import PositionedBlock
import Test.Hspec

createActionWithoutId :: String -> Double -> Double -> Double -> Double -> PositionedBlock
createActionWithoutId label x1 y1 x2 y2 =
  PositionedAction Nothing label x1 y1 x2 y2

createActionWithId :: String -> String -> Double -> Double -> Double -> Double -> PositionedBlock
createActionWithId idStr label x1 y1 x2 y2 =
  PositionedAction (Just (ID idStr)) label x1 y1 x2 y2

createFork :: String -> Double -> Double -> Double -> Double -> PositionedBlock
createFork label x1 y1 x2 y2 =
  PositionedFork Nothing label [] [] Nothing x1 y1 x2 y2

specShow :: Spec
specShow = describe "show" $ do
  context "PositionedAction" $ do
    it "displays an Action block without an ID" $ do
      let block = createActionWithoutId "-" 0.0 1.0 0.0 1.0
      show block `shouldBe` "Action \"-\" [0.0, 1.0, 0.0, 1.0]"

    it "displays an Action block with an ID" $ do
      let block = createActionWithId "1" "1" 2.0 3.0 2.0 3.0
      show block `shouldBe` "Action \"1\" [2.0, 3.0, 2.0, 3.0]"

  context "PositionedFork" $ do
    -- Verify that Fork blocks display their label and coordinates
    it "displays a Fork block" $ do
      let block = createFork "-" 4.0 5.0 4.0 5.0
      show block `shouldBe` "Fork \"-\" [4.0, 5.0, 4.0, 5.0]"
