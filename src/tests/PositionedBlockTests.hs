module PositionedBlockTests where

import Blocks
import ID
import PositionedBlock
import Test.Hspec

specShow :: Spec
specShow = describe "show" $ do
  it "shows various positioned blocks" $ do
    let block1 = PositionedAction Nothing "-" 0.0 1.0 0.0 1.0
    let block2 = PositionedAction (Just (ID "1")) "1" 2.0 3.0 2.0 3.0
    let block3 = PositionedFork Nothing "-" [] [] Nothing 4.0 5.0 4.0 5.0
    show block1 `shouldBe` "Action \"-\" [0.0, 1.0]"
    show block2 `shouldBe` "Action \"1\" [2.0, 3.0]"
    show block3 `shouldBe` "Fork \"-\" [4.0, 5.0]"
