module PositionedBlock where

import ID

data PositionedBlock
  = PositionedStartTerminator Double Double
  | PositionedAction (Maybe ID) String Double Double
  | PositionedHeadline (Maybe ID) String Double Double
  | PositionedAddress (Maybe ID) String Double Double
  | PositionedFork (Maybe ID) String [PositionedBlock] [PositionedBlock] (Maybe ID) Double Double
  | PositionedEndTerminator Double Double

instance Show PositionedBlock where
  show (PositionedStartTerminator x y) = "StrartTerminator [" <> show x <> ", " <> show y <> "]"
  show (PositionedAction _i c x y) = "Action \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedHeadline _i c x y) = "Headline \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedAddress _i c x y) = "Address \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedFork _i c _l _r _gCId x y) = "Fork \"" <> c <> "\" [" <> show x <> ", " <> show y <> "]"
  show (PositionedEndTerminator x y) = "EndTerminator [" <> show x <> ", " <> show y <> "]"

getPosition :: PositionedBlock -> (Double, Double)
getPosition (PositionedStartTerminator x y) = (x, y)
getPosition (PositionedAction _i _c x y) = (x, y)
getPosition (PositionedHeadline _i _c x y) = (x, y)
getPosition (PositionedAddress _i _c x y) = (x, y)
getPosition (PositionedFork _i _c _l _r _gCId x y) = (x, y)
getPosition (PositionedEndTerminator x y) = (x, y)

getContent :: PositionedBlock -> String
getContent (PositionedStartTerminator _x _y) = "start"
getContent (PositionedAction _i c _x _y) = c
getContent (PositionedHeadline _i c _x _y) = c
getContent (PositionedAddress _i c _x _y) = c
getContent (PositionedFork _i c _l _r _gCId _x _y) = c
getContent (PositionedEndTerminator _x _y) = "end"
