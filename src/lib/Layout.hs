module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import PositionedBlock

position'' :: Block -> Double -> Double -> (PositionedBlock, Double, Double)
position'' (Fork i c l r gCId) x y =
  let (positionedLeft, lMaxX, lMinY) = position' l x (y - defaultBoundingBoxHeight)
      (positionedRight, rMaxX, rMinY) = position' r lMaxX (y - defaultBoundingBoxHeight)
   in (PositionedFork i c positionedLeft positionedRight gCId x y, rMaxX, min lMinY rMinY)
position'' StartTerminator x y = (PositionedStartTerminator x y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)
position'' (Action i c) x y = (PositionedAction i c x y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)
position'' (Headline i c) x y = (PositionedHeadline i c x y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)
position'' (Address i c) x y = (PositionedAddress i c x y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)
position'' EndTerminator x y = (PositionedEndTerminator x y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)

position' :: [Block] -> Double -> Double -> ([PositionedBlock], Double, Double)
position' blocks x y =
  let (_finalX, _finalY, finalPositionedBlocks, finalMaxX, finalMinY) =
        foldr
          ( \b (accuX, accuY, accuPositionedBlocks, accuMaxX, accuMinY) ->
              let (positionedBlock, maxX, minY) = position'' b accuX accuY
               in ( accuX,
                    minY,
                    positionedBlock : accuPositionedBlocks,
                    max maxX accuMaxX,
                    min minY accuMinY
                  )
          )
          (x, y, [], x, y)
          blocks
   in (finalPositionedBlocks, finalMaxX, finalMinY)

position :: [[Block]] -> Double -> Double -> [[PositionedBlock]]
position skewers x y =
  let (_finalMaxX, finalPositionedBlocks) =
        foldr
          ( \skewer (accuMaxX, accuPositionedBlocks) ->
              let (positionedSkewer, maxX, minY) = position' skewer accuMaxX y
               in (maxX, positionedSkewer : accuPositionedBlocks)
          )
          (x, [])
          skewers
   in finalPositionedBlocks
