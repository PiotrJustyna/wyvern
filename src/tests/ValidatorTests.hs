module ValidatorTests where

import Blocks (Block (..))
import ID
import Test.Hspec
import Validator (categorizeId, findIncorrectGammaConnections', findIncorrectGammaConnections'')

-- ============================================================================
-- Helper Functions for Test Setup
-- ============================================================================

-- | Create a pair of ID lists for testing ID categorization
-- Returns (uniqueIDs, duplicatedIDs)
makeIdLists :: [String] -> [String] -> ([ID], [ID])
makeIdLists uniqueIds duplicatedIds = (map ID uniqueIds, map ID duplicatedIds)

-- | Create a fork block with an optional gamma connection ID
makeForkWithGamma :: Maybe String -> Block
makeForkWithGamma gammaId = Fork Nothing "" [] [] (fmap ID gammaId)

-- | Create a simple action block (used for multi-block tests)
makeActionBlock :: Block
makeActionBlock = Action Nothing ""

-- ============================================================================
-- Test Suites
-- ============================================================================

-- | Tests for categorizeId function
-- Validates the behavior of ID categorization into unique and duplicate lists
specCategorizeId :: Spec
specCategorizeId = describe "categorizeId" $ do
  -- When processing a new ID that hasn't been seen before
  context "with a new unique ID" $ do
    it "should add the ID to the unique list" $ do
      let (ids, duplicatedIds) = makeIdLists ["1", "2"] ["1", "2"]
      let (resultIds, resultDuplicatedIds) = categorizeId (ID "3") (ids, duplicatedIds)
      length resultIds `shouldBe` 3
      resultIds `shouldContain` [ID "3"]
      -- Duplicated IDs should remain unchanged
      resultDuplicatedIds `shouldBe` duplicatedIds

    it "should preserve the original duplicated list" $ do
      let (ids, duplicatedIds) = makeIdLists ["1", "2"] ["1", "2"]
      let (_, resultDuplicatedIds) = categorizeId (ID "3") (ids, duplicatedIds)
      resultDuplicatedIds `shouldBe` duplicatedIds

  -- When an ID is encountered for the second time
  context "when an ID is seen a second time" $ do
    it "should move the ID from unique to duplicated list" $ do
      let (ids, duplicatedIds) = makeIdLists ["1", "2"] []
      let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
      -- Original unique list size should remain the same
      length resultIds `shouldBe` 2
      -- Duplicated list should grow by one
      length resultDuplicatedIds `shouldBe` 1
      resultDuplicatedIds `shouldContain` [ID "1"]

  -- When an ID is already in the duplicated list
  context "when an ID is already duplicated" $ do
    it "should leave it unchanged in the duplicated list" $ do
      let (ids, duplicatedIds) = makeIdLists ["1", "2"] ["1"]
      let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
      length resultIds `shouldBe` 2
      length resultDuplicatedIds `shouldBe` 1
      resultDuplicatedIds `shouldBe` [ID "1"]

    it "should not affect the unique list" $ do
      let (ids, duplicatedIds) = makeIdLists ["1", "2"] ["1"]
      let (resultIds, _) = categorizeId (ID "1") (ids, duplicatedIds)
      resultIds `shouldBe` ids

  -- When starting with empty ID lists
  context "with empty starting lists" $ do
    it "should add a new ID to the unique list" $ do
      let (ids, duplicatedIds) = makeIdLists [] []
      let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
      length resultIds `shouldBe` 1
      length resultDuplicatedIds `shouldBe` 0
      resultIds `shouldContain` [ID "1"]

    it "should keep the duplicated list empty" $ do
      let (ids, duplicatedIds) = makeIdLists [] []
      let (_, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
      length resultDuplicatedIds `shouldBe` 0

-- | Tests for findIncorrectGammaConnections'' function
-- Validates gamma connection ID checking for individual blocks
specFindIncorrectGammaConnections'' :: Spec
specFindIncorrectGammaConnections'' = describe "findIncorrectGammaConnections''" $ do
  context "when validating fork gamma connections" $ do
    -- When a fork's gamma connection ID exists in the valid set
    context "with a valid gamma connection" $ do
      it "should return an empty list" $ do
        let fork = makeForkWithGamma (Just "1")
        let validIds = [ID "1", ID "2"]
        let result = findIncorrectGammaConnections'' fork [] validIds
        length result `shouldBe` 0

      it "should accept any ID from the valid set" $ do
        let fork = makeForkWithGamma (Just "2")
        let validIds = [ID "1", ID "2"]
        let result = findIncorrectGammaConnections'' fork [] validIds
        result `shouldBe` []

    -- When a fork's gamma connection ID is not in the valid set
    context "with an invalid gamma connection" $ do
      it "should return a list containing the invalid ID" $ do
        let fork = makeForkWithGamma (Just "3")
        let validIds = [ID "1", ID "2"]
        let result = findIncorrectGammaConnections'' fork [] validIds
        length result `shouldBe` 1
        result `shouldContain` [ID "3"]

      it "should detect ID that does not exist in valid set" $ do
        let fork = makeForkWithGamma (Just "999")
        let validIds = [ID "1", ID "2"]
        let result = findIncorrectGammaConnections'' fork [] validIds
        result `shouldContain` [ID "999"]

-- | Tests for findIncorrectGammaConnections' function
-- Validates gamma connection ID checking across multiple blocks
specFindIncorrectGammaConnections' :: Spec
specFindIncorrectGammaConnections' = describe "findIncorrectGammaConnections'" $ do
  context "when processing multiple blocks" $ do
    it "should combine all incorrect IDs found across blocks" $ do
      -- Test scenario: process a fork with invalid gamma connection and an action block
      let fork = makeForkWithGamma (Just "3")
      let action = makeActionBlock
      let blocks = [fork, action]
      let validIds = [ID "1", ID "2"]
      let result = findIncorrectGammaConnections' blocks [] validIds
      -- Should find one incorrect ID (the fork's gamma connection)
      length result `shouldBe` 1
      result `shouldContain` [ID "3"]

    it "should handle mixed block types" $ do
      let fork = makeForkWithGamma (Just "3")
      let action = makeActionBlock
      -- Verify that action block doesn't interfere with results
      let result = findIncorrectGammaConnections' [fork, action] [] [ID "1", ID "2"]
      result `shouldContain` [ID "3"]
