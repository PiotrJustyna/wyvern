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
