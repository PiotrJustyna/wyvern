module Validator where

import Blocks (Block (..), getIdentifier)
import ID (ID)

categorizeId :: ID -> ([ID], [ID]) -> ([ID], [ID])
categorizeId i (ids, duplicatedIds) =
  if i `elem` ids
    then
      ( ids,
        if i `elem` duplicatedIds
          then duplicatedIds
          else i : duplicatedIds
      )
    else (i : ids, duplicatedIds)

-- Duplicated IDs:

findDuplicatedIDs'' :: Block -> ([ID], [ID]) -> ([ID], [ID])
findDuplicatedIDs'' (Fork Nothing _c l r _gCId) (ids, duplicatedIds) =
  let left@(_lIds, _lDuplicatedIds) = findDuplicatedIDs' l (ids, duplicatedIds)
      right@(_rIds, _rDuplicatedIds) = findDuplicatedIDs' r left
   in right
findDuplicatedIDs'' (Fork (Just i) _c l r _gCId) (ids, duplicatedIds) =
  let left@(_lIds, _lDuplicatedIds) = findDuplicatedIDs' l $ categorizeId i (ids, duplicatedIds)
      right@(_rIds, _rDuplicatedIds) = findDuplicatedIDs' r left
   in right
findDuplicatedIDs'' b (ids, duplicatedIds) = case getIdentifier b of
  Just i -> categorizeId i (ids, duplicatedIds)
  Nothing -> (ids, duplicatedIds)

findDuplicatedIDs' :: [Block] -> ([ID], [ID]) -> ([ID], [ID])
findDuplicatedIDs' blocks ids = foldr findDuplicatedIDs'' ids blocks

findDuplicatedIDs :: [[Block]] -> ([ID], [ID])
findDuplicatedIDs = foldr findDuplicatedIDs' ([], [])

-- Incorrect gamma connections:

findIncorrectGammaConnections'' :: Block -> [ID] -> [ID] -> [ID]
findIncorrectGammaConnections'' (Fork _ _c l r (Just gCId)) incorrectGCIds allIds =
  let newIcorrectIds = if gCId `elem` allIds then incorrectGCIds else gCId : incorrectGCIds
      lIncorrectIds = findIncorrectGammaConnections' l newIcorrectIds allIds
   in findIncorrectGammaConnections' r lIncorrectIds allIds
findIncorrectGammaConnections'' _ incorrectGCIds _allIds = incorrectGCIds

findIncorrectGammaConnections' :: [Block] -> [ID] -> [ID] -> [ID]
findIncorrectGammaConnections' blocks incorrectGCIds allIds =
  foldr (\b accuIncorrectIds -> findIncorrectGammaConnections'' b accuIncorrectIds allIds) incorrectGCIds blocks

findIncorrectGammaConnections :: [[Block]] -> [ID] -> [ID]
findIncorrectGammaConnections blocks allIds =
  foldr (\bs accuIncorrectIds -> findIncorrectGammaConnections' bs accuIncorrectIds allIds) [] blocks

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

validate :: [[Block]] -> Either [[Block]] ([ID], [ID])
validate blocks =
  let (allIds, duplicatedIds) = findDuplicatedIDs blocks
      incorrectGCIds = findIncorrectGammaConnections blocks allIds
   in if isEmpty duplicatedIds && isEmpty incorrectGCIds
        then Left blocks
        else Right (duplicatedIds, incorrectGCIds)
