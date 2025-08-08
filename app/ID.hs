module ID where

newtype ID =
  ID String
  deriving (Show)

instance Eq ID where
  (==) (ID a) (ID b) = a == b

instance Ord ID where
  compare (ID a) (ID b) = compare a b
  (<) (ID a) (ID b) = a < b
  (>=) (ID a) (ID b) = a >= b
  (>) (ID a) (ID b) = a > b
  (<=) (ID a) (ID b) = a <= b
  min (ID a) (ID b) = ID $ min a b
  max (ID a) (ID b) = ID $ max a b
