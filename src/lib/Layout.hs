module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import PositionedBlock

position'' :: Block -> Double -> Double -> PositionedBlock
position'' (Fork i c l r gCId) x y =
  let (positionedLeft, lMaxX, lMinY) = position' l x (y - defaultBoundingBoxHeight)
      (positionedRight, rMaxX, rMinY) = position' r (max lMaxX (x + defaultBoundingBoxWidth)) (y - defaultBoundingBoxHeight)
   in (PositionedFork i c (Prelude.reverse positionedLeft) (Prelude.reverse positionedRight) gCId x y rMaxX (min lMinY rMinY))
position'' StartTerminator x y = PositionedStartTerminator x y (x + defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight)
position'' (Action i c) x y = PositionedAction i c x y (x + defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight)
position'' (Headline i c) x y = PositionedHeadline i c x y (x + defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight)
position'' (Address i c) x y = PositionedAddress i c x y (x + defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight)
position'' EndTerminator x y = PositionedEndTerminator x y (x + defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight)

position' :: [Block] -> Double -> Double -> ([PositionedBlock], Double, Double)
position' blocks x y =
  let (_finalX, _finalY, finalPositionedBlocks, finalMaxX, finalMinY) =
        foldr
          ( \b (accuX, accuY, accuPositionedBlocks, accuMaxX, accuMinY) ->
              let positionedBlock = position'' b accuX accuY
                  (_x, _y, maxX, minY) = getPosition positionedBlock
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
              let (positionedSkewer, maxX, _minY) = position' skewer accuMaxX y
               in (maxX, (Prelude.reverse positionedSkewer) : accuPositionedBlocks)
          )
          (x, [])
          skewers
   in finalPositionedBlocks

connections'' :: PositionedBlock -> [((Double, Double), (Double, Double))]
connections'' (PositionedFork _i _c l r _gCId x y maxX minY) =
  let lc = case l of
        [] -> ((x, y), (x, minY))
        (b : _) ->
          let (lx, ly, _lMaxX, _lMinY) = getPosition b
           in ((x, y), (lx, ly))
      rc = case r of
        [] -> [((x, y), (maxX, y)), ((maxX, y), (maxX, minY))]
        (b : _) ->
          let (rx, ry, rmaxX, rMinY) = getPosition b
           in [((x, y), (rx, y)), ((rx, y), (rx, ry))]
   in lc : (rc <> connections' l <> connections' r)
connections'' _ = []

connections' :: [PositionedBlock] -> [((Double, Double), (Double, Double))]
connections' [] = []
connections' [pB] = connections'' pB
connections' (pB1 : pB2 : pBs) =
  case pB1 of
    (PositionedFork _i _c l _r _gCId x1 y1 maxX1 minY1) ->
      let position2@(x2, y2, maxX2, minY2) = getPosition pB2
          lConnection = case l of
            [] -> []
            _ ->
              let lastL@(_xLastL, yLastL, _maxXLastL, _minYLastL) = getPosition (last l)
               in [((x1 + 1.2, yLastL), (x2 + 1.2, y2))]
       in lConnection <> connections'' pB1 <> connections' (pB2 : pBs)
    _ -> connections' (pB2 : pBs)

-- let position1@(x1, y1, maxX1, minY1) = getPosition pB1
--     position2@(x2, y2, maxX2, minY2) = getPosition pB2
--  in ((x1, y1), (x2 + 0.5, y2)) : connections' (pB2 : pBs) -- connections'' pB1

connections :: [[PositionedBlock]] -> [((Double, Double), (Double, Double))]
connections = foldr (\pBs accu -> connections' pBs <> accu) []
