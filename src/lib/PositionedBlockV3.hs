module PositionedBlockV3 where

import Constants
import ID

data PositionedBlockV3
  = PositionedStartTerminatorV3 Int Double Double Double Double
  | PositionedActionV3 (Maybe ID) Int String Double Double Double Double Double
  | PositionedHeadlineV3 (Maybe ID) Int String Double Double Double Double
  | PositionedAddressV3 (Maybe ID) Int String Double Double Double Double
  | PositionedForkV3 (Maybe ID) Int String [PositionedBlockV3] [PositionedBlockV3] (Maybe ID) Double Double Double Double Double
  | PositionedEndTerminatorV3 Int Double Double Double Double

instance Show PositionedBlockV3 where
  show (PositionedStartTerminatorV3 _pId x y _hExpansion _vExpansion) = "StartTerminator"
  show pB@(PositionedActionV3 i pId c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) =
    let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 pB
     in "Action [" <> show i <> "|" <> show pId <> "] x: " <> show x <> " y: " <> show y <> " maxX: " <> show maxX <> " minY: " <> show minY
  show (PositionedHeadlineV3 _i _pId c x y _hExpansion _vExpansion) = "PositionedHeadlineV3"
  show (PositionedAddressV3 _i _pId c x y _hExpansion _vExpansion) = "PositionedAddressV3"
  show pB@(PositionedForkV3 i pId c l _r _gCId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) =
    let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 pB
     in "Fork [" <> show i <> "|" <> show pId <> "] x: " <> show x <> " y: " <> show y <> " maxX: " <> show maxX <> " minY: " <> show minY
  show (PositionedEndTerminatorV3 _pId x y _hExpansion _vExpansion) = "PositionedEndTerminatorV3"

getPositionV3 :: PositionedBlockV3 -> (Double, Double, Double, Double, Double, Double, Double)
getPositionV3 (PositionedStartTerminatorV3 _pId x y hExpansion vExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, hExpansion, vExpansion, 0.0)
getPositionV3 (PositionedActionV3 _i _pId _c x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) = (x, y, x + defaultBoundingBoxWidth + horizontalExpansion, y - defaultBoundingBoxHeight - verticalBottomExpansion - verticalTopExpansion, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion)
getPositionV3 (PositionedHeadlineV3 _i _pId _c x y hExpansion vExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, hExpansion, vExpansion, 0.0)
getPositionV3 (PositionedAddressV3 _i _pId _c x y hExpansion vExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, hExpansion, vExpansion, 0.0)
getPositionV3 (PositionedForkV3 _i _pId _c l r _gCId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) =
  let (maxX, rMinY) = case r of
        [] -> (x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)
        _ -> (getMaxX r, getMinY r)
      lMinY = case l of
        [] -> y - defaultBoundingBoxHeight
        _ -> getMinY l
   in (x, y, maxX, min lMinY rMinY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion)
getPositionV3 (PositionedEndTerminatorV3 _pId x y hExpansion vExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, hExpansion, vExpansion, 0.0)

getMaxX :: [PositionedBlockV3] -> Double
getMaxX =
  foldr
    ( \b accu ->
        let (_x, _y, maxX, _minY, horizontalExpansion, _verticalBottomExpansion, _verticalTopExpansion) = getPositionV3 b
         in max maxX accu
    )
    0.0

getMinY :: [PositionedBlockV3] -> Double
getMinY =
  foldr
    ( \b accu ->
        let (_x, _y, _maxX, minY, _horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 b
         in min minY accu
    )
    0.0
