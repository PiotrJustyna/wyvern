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
  show (PositionedStartTerminator x y _maxX _minY) = "StrartTerminator [" <> show x <> ", " <> show y <> "]"
  show (PositionedAction _i c x y _maxX _minY) = "Action \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedHeadline _i c x y _maxX _minY) = "Headline \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedAddress _i c x y _maxX _minY) = "Address \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedFork _i c _l _r _gCId x y _maxX _minY) = "Fork \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedEndTerminator x y _maxX _minY) = "EndTerminator [" <> show x <> ", " <> show y <> "]"

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
