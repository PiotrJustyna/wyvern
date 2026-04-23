module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import PositionedBlock

position' :: Block -> Double -> Double -> PositionedBlock
position' (Fork i c l r gCId) x y =
  let positionedLeft = position l x (y - defaultBoundingBoxHeight)
      positionedRight = position r (x + defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight)
   in PositionedFork i c positionedLeft positionedRight gCId x y
position' StartTerminator x y = PositionedStartTerminator x y
position' (Action i c) x y = PositionedAction i c x y
position' (Headline i c) x y = PositionedHeadline i c x y
position' (Address i c) x y = PositionedAddress i c x y
position' EndTerminator x y = PositionedEndTerminator x y

position :: [Block] -> Double -> Double -> [PositionedBlock]
position blocks x y =
  let (_finalMinY, _finalMaxX, finalPositionedBlocks) =
        foldl
          ( \(accuMaxX, accuMinY, accuPositionedBlocks) b ->
              ( accuMaxX,
                accuMinY - defaultBoundingBoxHeight,
                position' b accuMaxX accuMinY : accuPositionedBlocks
              )
          )
          (x, y, [])
          blocks
   in finalPositionedBlocks
