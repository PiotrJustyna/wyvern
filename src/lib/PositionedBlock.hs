module PositionedBlock where

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

-- ?? TODO: questionable - does not set new positions of fork's branches
-- also: min y need to reflect the new position
setPosition :: PositionedBlock -> Double -> Double -> PositionedBlock
setPosition (PositionedStartTerminator x y maxX minY) newx newy = (PositionedStartTerminator newx newy maxX minY)
setPosition (PositionedAction _i _c x y maxX minY) newx newy = (PositionedAction _i _c newx newy maxX minY)
setPosition (PositionedHeadline _i _c x y maxX minY) newx newy = (PositionedHeadline _i _c newx newy maxX minY)
setPosition (PositionedAddress _i _c x y maxX minY) newx newy = (PositionedAddress _i _c newx newy maxX minY)
setPosition (PositionedFork _i _c _l _r _gCId x y maxX minY) newx newy = (PositionedFork _i _c _l _r _gCId newx newy maxX minY)
setPosition (PositionedEndTerminator x y maxX minY) newx newy = (PositionedEndTerminator newx newy maxX minY)

getContent :: PositionedBlock -> String
getContent (PositionedStartTerminator _x _y _maxX _minY) = "start"
getContent (PositionedAction _i c _x _y _maxX _minY) = c
getContent (PositionedHeadline _i c _x _y _maxX _minY) = c
getContent (PositionedAddress _i c _x _y _maxX _minY) = c
getContent (PositionedFork _i c _l _r _gCId _x _y _maxX _minY) = c
getContent (PositionedEndTerminator _x _y _maxX _minY) = "end"
