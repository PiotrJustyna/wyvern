module ValidatorTests where

import Blocks (Block (..))
import ID
import Test.Hspec
import Validator (categorizeId, findIncorrectGammaConnections', findIncorrectGammaConnections'')

specCategorizeId :: Spec
specCategorizeId = describe "categorizeId" $ do
  it "adds a new unique ID to the unique list" $ do
    let ids = [ID "1", ID "2"]
    let duplicatedIds = [ID "1", ID "2"]
    let (resultIds, resultDuplicatedIds) = categorizeId (ID "3") (ids, duplicatedIds)
    length resultIds `shouldBe` 3
    resultIds `shouldContain` [ID "3"]
    resultDuplicatedIds `shouldBe` duplicatedIds

  it "moves an ID to duplicated list when seen a second time" $ do
    let ids = [ID "1", ID "2"]
    let duplicatedIds = []
    let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
    length resultIds `shouldBe` 2
    length resultDuplicatedIds `shouldBe` 1
    resultDuplicatedIds `shouldContain` [ID "1"]

  it "leaves an ID unchanged if already in duplicated list" $ do
    let ids = [ID "1", ID "2"]
    let duplicatedIds = [ID "1"]
    let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
    length resultIds `shouldBe` 2
    length resultDuplicatedIds `shouldBe` 1
    resultDuplicatedIds `shouldBe` [ID "1"]

  it "adds a new ID to unique list with empty starting lists" $ do
    let ids = []
    let duplicatedIds = []
    let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
    length resultIds `shouldBe` 1
    length resultDuplicatedIds `shouldBe` 0
    resultIds `shouldContain` [ID "1"]

specFindIncorrectGammaConnections'' :: Spec
specFindIncorrectGammaConnections'' = describe "findIncorrectGammaConnections''" $ do
  it "detects a fork's gamma connection id is correct" $ do
    let fork = Fork Nothing "" [] [] (Just (ID "1"))
    let result = findIncorrectGammaConnections'' fork [] [ID "1", ID "2"]
    length result `shouldBe` 0

  it "detects a fork's gamma connection id is incorrect" $ do
    let fork = Fork Nothing "" [] [] (Just (ID "3"))
    let result = findIncorrectGammaConnections'' fork [] [ID "1", ID "2"]
    length result `shouldBe` 1
    result `shouldContain` [ID "3"]

specFindIncorrectGammaConnections' :: Spec
specFindIncorrectGammaConnections' = describe "specFindIncorrectGammaConnections'" $ do
  it "combines all incorrect ids found in a list of blocks" $ do
    let fork = Fork Nothing "" [] [] (Just (ID "3"))
    let action = Action Nothing ""
    let result = findIncorrectGammaConnections' [fork, action] [] [ID "1", ID "2"]
    length result `shouldBe` 1
    result `shouldContain` [ID "3"]
