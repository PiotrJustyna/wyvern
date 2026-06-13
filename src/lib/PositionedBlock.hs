module PositionedBlock where

import Data.Map (Map, empty, foldrWithKey, insert, insertWith, lookup, member)
import ID

data PositionedBlock
  = PositionedStartTerminator Double Double Double Double
  | PositionedAction (Maybe ID) String Double Double Double Double
  | PositionedHeadline (Maybe ID) String Double Double Double Double
  | PositionedAddress (Maybe ID) String Double Double Double Double
  | PositionedFork (Maybe ID) String [PositionedBlock] [PositionedBlock] (Maybe ID) Double Double Double Double
  | PositionedEndTerminator Double Double Double Double

instance Show PositionedBlock where
  show (PositionedStartTerminator x y maxX minY) = "StrartTerminator [" <> show x <> ", " <> show y <> ", " <> show maxX <> ", " <> show minY <> "]"
  show (PositionedAction _i c x y maxX minY) = "Action \"" <> c <> "\" [" <> show x <> ", " <> show y <> ", " <> show maxX <> ", " <> show minY <> "]"
  show (PositionedHeadline _i c x y maxX minY) = "Headline \"" <> c <> "\" [" <> show x <> ", " <> show y <> ", " <> show maxX <> ", " <> show minY <> "]"
  show (PositionedAddress _i c x y maxX minY) = "Address \"" <> c <> "\" [" <> show x <> ", " <> show y <> ", " <> show maxX <> ", " <> show minY <> "]"
  show (PositionedFork _i c _l _r _gCId x y maxX minY) = "Fork \"" <> c <> "\" [" <> show x <> ", " <> show y <> ", " <> show maxX <> ", " <> show minY <> "]"
  show (PositionedEndTerminator x y maxX minY) = "EndTerminator [" <> show x <> ", " <> show y <> ", " <> ", " <> show maxX <> ", " <> show minY <> "]"

getPosition :: PositionedBlock -> (Double, Double, Double, Double)
getPosition (PositionedStartTerminator x y maxX minY) = (x, y, maxX, minY)
getPosition (PositionedAction _i _c x y maxX minY) = (x, y, maxX, minY)
getPosition (PositionedHeadline _i _c x y maxX minY) = (x, y, maxX, minY)
getPosition (PositionedAddress _i _c x y maxX minY) = (x, y, maxX, minY)
getPosition (PositionedFork _i _c _l _r _gCId x y maxX minY) = (x, y, maxX, minY)
getPosition (PositionedEndTerminator x y maxX minY) = (x, y, maxX, minY)

getContent :: PositionedBlock -> String
getContent (PositionedStartTerminator _x _y _maxX _minY) = "start"
getContent (PositionedAction _i c _x _y _maxX _minY) = c
getContent (PositionedHeadline _i c _x _y _maxX _minY) = c
getContent (PositionedAddress _i c _x _y _maxX _minY) = c
getContent (PositionedFork _i c _l _r _gCId _x _y _maxX _minY) = c
getContent (PositionedEndTerminator _x _y _maxX _minY) = "end"

getId :: PositionedBlock -> Maybe ID
getId (PositionedStartTerminator _x _y _maxX _minY) = Nothing
getId (PositionedAction i _c _x _y _maxX _minY) = i
getId (PositionedHeadline i _c _x _y _maxX _minY) = i
getId (PositionedAddress i _c _x _y _maxX _minY) = i
getId (PositionedFork i _c _l _r _gCId _x _y _maxX _minY) = i
getId (PositionedEndTerminator _x _y _maxX _minY) = Nothing

toMap'' :: Map ID (Double, Double, Double, Double, Double, Double) -> PositionedBlock -> Map ID (Double, Double, Double, Double, Double, Double)
toMap'' m (PositionedFork i _c l r _gCId x y maxX minY) =
  let questionMap = case i of
        Nothing -> m
        Just i' -> Data.Map.insert i' (x, y, maxX, minY, 0.0, 0.0) m
      lMap = toMap' questionMap l
      rMap = toMap' lMap r
   in rMap
toMap'' m b =
  let i = getId b
      (x, y, maxX, minY) = getPosition b
   in case i of
        Nothing -> m
        Just i' -> Data.Map.insert i' (x, y, maxX, minY, 0.0, 0.0) m

toMap' :: Map ID (Double, Double, Double, Double, Double, Double) -> [PositionedBlock] -> Map ID (Double, Double, Double, Double, Double, Double)
toMap' = foldl toMap''

toMap :: [[PositionedBlock]] -> Map ID (Double, Double, Double, Double, Double, Double)
toMap = foldl (\accu x -> toMap' accu x) empty

toMapMicro'' :: Map ID Double -> PositionedBlock -> Map ID Double
toMapMicro'' m (PositionedFork i _c l r _gCId x y maxX minY) =
  let questionMap = case i of
        Nothing -> m
        Just i' -> Data.Map.insert i' y m
      lMap = toMapMicro' questionMap l
      rMap = toMapMicro' lMap r
   in rMap
toMapMicro'' m b =
  let i = getId b
      (x, y, maxX, minY) = getPosition b
   in case i of
        Nothing -> m
        Just i' -> Data.Map.insert i' y m

toMapMicro' :: Map ID Double -> [PositionedBlock] -> Map ID Double
toMapMicro' = foldl toMapMicro''

toMapMicro :: [[PositionedBlock]] -> Map ID Double
toMapMicro = foldl (\accu x -> toMapMicro' accu x) empty

gammaConnectionDestinations'' :: Map ID Double -> PositionedBlock -> Map ID Double
gammaConnectionDestinations'' m (PositionedFork i _c l r gCId x y maxX minY) =
  let questionMap = case gCId of
        Nothing -> m
        Just gCId' -> Data.Map.insert gCId' y m
      lMap = gammaConnectionDestinations' questionMap l
      rMap = gammaConnectionDestinations' lMap r
   in rMap
gammaConnectionDestinations'' m _ = m

gammaConnectionDestinations' :: Map ID Double -> [PositionedBlock] -> Map ID Double
gammaConnectionDestinations' = foldl gammaConnectionDestinations''

-- TODO:
-- 1. this cannot be a map - there could be duplicates and that's fine
-- 2. positioned block with most gamma connections leading to it should win not to duplicate y shifts unnecessarily
gammaConnectionDestinations :: [[PositionedBlock]] -> Map ID Double
gammaConnectionDestinations = foldl (\accu x -> gammaConnectionDestinations' accu x) empty

qwe :: Map ID Double -> Map Double (ID, Int)
qwe = Data.Map.foldrWithKey (\i y accu -> Data.Map.insertWith (\(iNew, counterNew) (iOld, counterOld) -> (iNew, counterOld + counterNew)) y (i, 0) accu) empty

extractGammaConnections'' :: Map ID Double -> PositionedBlock -> [(ID, Double)]
extractGammaConnections'' m (PositionedFork _i _c l r gCId x y maxX minY) =
  let questionConnection =
        case gCId of
          Nothing -> []
          (Just gCId') ->
            case Data.Map.lookup gCId' m of
              Nothing -> error $ "gamma connection id \"" <> show gCId' <> "\" does not exist in the collection of block identifiers: " <> show m
              (Just y) -> [(gCId', y)]
      leftBranchConnections = extractGammaConnections' m l
      rightBranchConnections = extractGammaConnections' m r
   in questionConnection <> leftBranchConnections <> rightBranchConnections
extractGammaConnections'' _m _positionedBlock = []

extractGammaConnections' :: Map ID Double -> [PositionedBlock] -> [(ID, Double)]
extractGammaConnections' destinations = foldl (\accu positionedBlock -> accu <> extractGammaConnections'' destinations positionedBlock) []

-- NOTE:
-- The name can be misleading but I intentionally want to leave it like this
-- to allow myself to restructure the function easier in the future.
-- I feel this function will return more details about gamma connections
-- but for now the returned type feels sufficient.
extractGammaConnections :: Map ID Double -> [[PositionedBlock]] -> [(ID, Double)]
extractGammaConnections destinations = foldl (\accu positionedBlocks -> accu <> extractGammaConnections' destinations positionedBlocks) []
