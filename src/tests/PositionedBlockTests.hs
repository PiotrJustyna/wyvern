module PositionedBlockTests where

import Blocks
import ID
import PositionedBlock
import Test.Hspec

-- | Helper function to create an Action block without an ID
-- Used to test Action blocks with the default "-" label
createActionWithoutId :: String -> Double -> Double -> Double -> Double -> PositionedBlock
createActionWithoutId label x1 y1 x2 y2 =
  PositionedAction Nothing label x1 y1 x2 y2

-- | Helper function to create an Action block with an ID
-- Used to test Action blocks that have been assigned an identifier
createActionWithId :: String -> String -> Double -> Double -> Double -> Double -> PositionedBlock
createActionWithId idStr label x1 y1 x2 y2 =
  PositionedAction (Just (ID idStr)) label x1 y1 x2 y2

-- | Helper function to create a Fork block
-- Used to test Fork block display with default empty branch lists
createFork :: String -> Double -> Double -> Double -> Double -> PositionedBlock
createFork label x1 y1 x2 y2 =
  PositionedFork Nothing label [] [] Nothing x1 y1 x2 y2

-- | Main specification for PositionedBlock show instances
-- Tests that blocks are formatted correctly when converted to strings
specShow :: Spec
specShow = describe "show" $ do
  -- Test suite for PositionedAction blocks
  context "PositionedAction" $ do
    -- Verify that Action blocks without IDs display their label and coordinates
    it "displays an Action block without an ID" $ do
      let block = createActionWithoutId "-" 0.0 1.0 0.0 1.0
      show block `shouldBe` "Action \"-\" [0.0, 1.0]"

    -- Verify that Action blocks with IDs display their label and coordinates
    it "displays an Action block with an ID" $ do
      let block = createActionWithId "1" "1" 2.0 3.0 2.0 3.0
      show block `shouldBe` "Action \"1\" [2.0, 3.0]"

  -- Test suite for PositionedFork blocks
  context "PositionedFork" $ do
    -- Verify that Fork blocks display their label and coordinates
    it "displays a Fork block" $ do
      let block = createFork "-" 4.0 5.0 4.0 5.0
      show block `shouldBe` "Fork \"-\" [4.0, 5.0]"
