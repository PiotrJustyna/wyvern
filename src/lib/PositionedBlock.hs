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
  show (PositionedAction _i _c x y) = "Action [" <> show x <> ", " <> show y <> "]"
  show (PositionedHeadline _i _c x y) = "Headline [" <> show x <> ", " <> show y <> "]"
  show (PositionedAddress _i _c x y) = "Address [" <> show x <> ", " <> show y <> "]"
  show (PositionedFork _i _c _l _r _gCId x y) = "Fork [" <> show x <> ", " <> show y <> "]"
  show (PositionedEndTerminator x y) = "EndTerminator [" <> show x <> ", " <> show y <> "]"

getPosition :: PositionedBlock -> (Double, Double)
getPosition (PositionedStartTerminator x y) = (x, y)
getPosition (PositionedAction _i _c x y) = (x, y)
getPosition (PositionedHeadline _i _c x y) = (x, y)
getPosition (PositionedAddress _i _c x y) = (x, y)
getPosition (PositionedFork _i _c _l _r _gCId x y) = (x, y)
getPosition (PositionedEndTerminator x y) = (x, y)
