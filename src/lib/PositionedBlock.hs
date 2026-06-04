module PositionedBlock where

import Data.Map (Map, empty, insert, lookup)
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

toMap'' :: Map ID (Double, Double, Double, Double) -> PositionedBlock -> Map ID (Double, Double, Double, Double)
toMap'' m (PositionedFork i _c l r _gCId x y maxX minY) =
  let questionMap = case i of
        Nothing -> m
        Just i' -> Data.Map.insert i' (x, y, maxX, minY) m
      lMap = toMap' questionMap l
      rMap = toMap' lMap r
   in rMap
toMap'' m b =
  let i = getId b
      (x, y, maxX, minY) = getPosition b
   in case i of
        Nothing -> m
        Just i' -> Data.Map.insert i' (x, y, maxX, minY) m

toMap' :: Map ID (Double, Double, Double, Double) -> [PositionedBlock] -> Map ID (Double, Double, Double, Double)
toMap' = foldl toMap''

toMap :: [[PositionedBlock]] -> Map ID (Double, Double, Double, Double)
toMap = foldl (\accu x -> toMap' accu x) empty
