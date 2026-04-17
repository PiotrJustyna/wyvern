module Validator where

import Blocks (Block, getIdentifier)
import ID (ID)

findDuplicatedIDs' :: [Block] -> ([ID], [ID]) -> ([ID], [ID])
findDuplicatedIDs' blocks ids =
  foldl
    ( \(accuIds, accuDuplicatedIds) b -> case getIdentifier b of
        Just i ->
          if i `elem` accuIds
            then
              ( accuIds,
                if i `elem` accuDuplicatedIds
                  then accuDuplicatedIds
                  else i : accuDuplicatedIds
              )
            else (i : accuIds, accuDuplicatedIds)
        Nothing -> (accuIds, accuDuplicatedIds)
    )
    ids
    blocks

findDuplicatedIDs :: [[Block]] -> ([ID], [ID])
findDuplicatedIDs = foldr findDuplicatedIDs' ([], [])
