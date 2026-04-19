module ValidatorTests where

import ID
import Test.HUnit
import Validator (categorizeId)

idCategorizedAsUnique :: Test
idCategorizedAsUnique = do
  let ids = [ID "1", ID "2"]
  let duplicatedIds = [ID "1", ID "2"]
  let (resultIds, resultDuplicatedIds) = categorizeId (ID "3") (ids, duplicatedIds)
  TestCase (assertEqual "ids count correct" 3 (length resultIds))

-- Test when an ID is seen for the second time (should move to duplicated list)
idMovedToDuplicated :: Test
idMovedToDuplicated = do
  let ids = [ID "1", ID "2"]
  let duplicatedIds = []
  let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
  TestCase $ do
    assertEqual "unique ids unchanged" 2 (length resultIds)
    assertEqual "id added to duplicated" 1 (length resultDuplicatedIds)
    assertEqual "duplicated contains correct id" True (ID "1" `elem` resultDuplicatedIds)

-- Test when an ID already exists in duplicated list (should remain unchanged)
idAlreadyInDuplicated :: Test
idAlreadyInDuplicated = do
  let ids = [ID "1", ID "2"]
  let duplicatedIds = [ID "1"]
  let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
  TestCase $ do
    assertEqual "unique ids unchanged" 2 (length resultIds)
    assertEqual "duplicated ids unchanged" 1 (length resultDuplicatedIds)

-- Test with empty initial lists (new ID should be added to unique list)
newIdWithEmptyLists :: Test
newIdWithEmptyLists = do
  let ids = []
  let duplicatedIds = []
  let (resultIds, resultDuplicatedIds) = categorizeId (ID "1") (ids, duplicatedIds)
  TestCase $ do
    assertEqual "id added to unique list" 1 (length resultIds)
    assertEqual "duplicated list remains empty" 0 (length resultDuplicatedIds)
    assertEqual "unique list contains correct id" True (ID "1" `elem` resultIds)
