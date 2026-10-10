module PositionedBlockV3 where

import Constants
import ID
import IntermediateBlock

data PositionedBlockV3
  = PositionedStartTerminatorV3 Int Double Double Double Double Double
  | PositionedEndTerminatorV3 Int Double Double Double Double Double
  | PositionedActionV3 (Maybe ID) Int String Double Double Double Double Double
  | PositionedHeadlineV3 (Maybe ID) Int String Double Double Double Double Double
  | PositionedAddressV3 (Maybe ID) Int String Double Double Double Double Double
  | PositionedForkV3 (Maybe ID) Int String [PositionedBlockV3] [PositionedBlockV3] (Maybe ID) Double Double Double Double Double

instance Show PositionedBlockV3 where
  show (PositionedStartTerminatorV3 _pId x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) = "StartTerminator"
  show (PositionedEndTerminatorV3 _pId x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) = "PositionedEndTerminatorV3"
  show pB@(PositionedActionV3 i pId c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) =
    let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 pB
     in "Action [" <> show i <> "|" <> show pId <> "] x: " <> show x <> " y: " <> show y <> " maxX: " <> show maxX <> " minY: " <> show minY
  show (PositionedHeadlineV3 _i _pId c x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) = "PositionedHeadlineV3"
  show (PositionedAddressV3 _i _pId c x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) = "PositionedAddressV3"
  show pB@(PositionedForkV3 i pId c l _r _gCId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) =
    let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 pB
     in "Fork [" <> show i <> "|" <> show pId <> "] x: " <> show x <> " y: " <> show y <> " maxX: " <> show maxX <> " minY: " <> show minY

getPositionV3 :: PositionedBlockV3 -> (Double, Double, Double, Double, Double, Double, Double)
getPositionV3 (PositionedStartTerminatorV3 _pId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion)
getPositionV3 (PositionedEndTerminatorV3 _pId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion)
getPositionV3 (PositionedActionV3 _i _pId _c x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) =
  ( x,
    y,
    x + defaultBoundingBoxWidth + horizontalExpansion,
    y - defaultBoundingBoxHeight - verticalBottomExpansion - verticalTopExpansion,
    horizontalExpansion,
    verticalBottomExpansion,
    verticalTopExpansion
  )
getPositionV3 (PositionedHeadlineV3 _i _pId _c x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion)
getPositionV3 (PositionedAddressV3 _i _pId _c x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion)
getPositionV3 (PositionedForkV3 _i _pId _c l r _gCId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion) =
  let (maxX, rMinY) = case r of
        [] -> (x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)
        _ -> (getMaxX r, getMinY r)
      lMinY = case l of
        [] -> y - defaultBoundingBoxHeight
        _ -> getMinY l
   in ( x,
        y,
        maxX + horizontalExpansion,
        (min lMinY rMinY) - verticalBottomExpansion,
        horizontalExpansion,
        verticalBottomExpansion,
        verticalTopExpansion
      )

getMaxX :: [PositionedBlockV3] -> Double
getMaxX =
  foldr
    ( \b accu ->
        let (_x, _y, maxX, _minY, _horizontalExpansion, _verticalBottomExpansion, _verticalTopExpansion) = getPositionV3 b
         in max maxX accu
    )
    0.0

getMinY :: [PositionedBlockV3] -> Double
getMinY =
  foldr
    ( \b accu ->
        let (_x, _y, _maxX, minY, _horizontalExpansion, _verticalBottomExpansion, _verticalTopExpansion) = getPositionV3 b
         in min minY accu
    )
    0.0

position'' :: IntermediateBlock -> Double -> Double -> PositionedBlockV3
position'' (IntermediateStartTerminator programmaticId horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y = PositionedStartTerminatorV3 programmaticId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion
position'' (IntermediateEndTerminator programmaticId horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y = PositionedEndTerminatorV3 programmaticId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion
position'' (IntermediateAction userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y = PositionedActionV3 userId programmaticId content x y horizontalExpansion verticalBottomExpansion verticalTopExpansion
position'' (IntermediateHeadline userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y = PositionedHeadlineV3 userId programmaticId content x y horizontalExpansion verticalBottomExpansion verticalTopExpansion
position'' (IntermediateAddress userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y = PositionedAddressV3 userId programmaticId content x y horizontalExpansion verticalBottomExpansion verticalTopExpansion
position'' (IntermediateFork userId programmaticId content leftBranch rightBranch gammaConnectionDestinationUserId horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y =
  let (positionedLeft, lMaxX, lMinY) = position' leftBranch x (y - defaultBoundingBoxHeight - verticalTopExpansion)
      (positionedRight, rMaxX, rMinY) = position' rightBranch lMaxX (y - defaultBoundingBoxHeight - verticalTopExpansion)
   in PositionedForkV3 userId programmaticId content (Prelude.reverse positionedLeft) (Prelude.reverse positionedRight) gammaConnectionDestinationUserId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion

position' :: [IntermediateBlock] -> Double -> Double -> ([PositionedBlockV3], Double, Double)
position' intermediateBlocks x y =
  let (_finalX, _finalY, finalPositionedBlocks, finalMaxX, finalMinY) =
        foldl
          ( \(accuX, accuY, accuPositionedBlocks, accuMaxX, accuMinY) iB ->
              let positionedBlock = position'' iB accuX accuY
                  (_x, _y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 positionedBlock
               in ( accuX,
                    minY,
                    positionedBlock : accuPositionedBlocks,
                    horizontalExpansion + max maxX accuMaxX,
                    (min minY accuMinY)
                  )
          )
          (x, y, [], x, y)
          intermediateBlocks
   in (finalPositionedBlocks, finalMaxX, finalMinY)

position :: [[IntermediateBlock]] -> Double -> Double -> [[PositionedBlockV3]]
position skewers x y =
  let (_finalMaxX, finalPositionedBlocks) =
        foldl
          ( \(accuMaxX, accuPositionedBlocks) skewer ->
              let (positionedSkewer, maxX, _minY) = position' skewer accuMaxX y
               in (maxX, (Prelude.reverse positionedSkewer) : accuPositionedBlocks)
          )
          (x, [])
          skewers
   in finalPositionedBlocks
