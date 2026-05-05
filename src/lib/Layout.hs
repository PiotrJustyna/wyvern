module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import PositionedBlock

position'' :: Block -> Double -> Double -> PositionedBlock
position'' (Fork i c l r gCId) x y =
  let (positionedLeft, lMaxX, lMinY) = position' l x (y - defaultBoundingBoxHeight * 0.5)
      (positionedRight, rMaxX, rMinY) = position' r (lMaxX + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)
   in (PositionedFork i c (Prelude.reverse positionedLeft) (Prelude.reverse positionedRight) gCId x y rMaxX (min lMinY rMinY))
position'' StartTerminator x y = PositionedStartTerminator x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)
position'' (Action i c) x y = PositionedAction i c x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)
position'' (Headline i c) x y = PositionedHeadline i c x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)
position'' (Address i c) x y = PositionedAddress i c x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)
position'' EndTerminator x y = PositionedEndTerminator x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)

position' :: [Block] -> Double -> Double -> ([PositionedBlock], Double, Double)
position' blocks x y =
  let (_finalX, _finalY, finalPositionedBlocks, finalMaxX, finalMinY) =
        foldl
          ( \(accuX, accuY, accuPositionedBlocks, accuMaxX, accuMinY) b ->
              let positionedBlock = position'' b accuX (accuY - defaultBoundingBoxHeight * 0.5)
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
        foldl
          ( \(accuMaxX, accuPositionedBlocks) skewer ->
              let (positionedSkewer, maxX, _minY) = position' skewer accuMaxX y
               in (maxX + defaultBoundingBoxWidth * 0.5, (Prelude.reverse positionedSkewer) : accuPositionedBlocks)
          )
          (x, [])
          skewers
   in finalPositionedBlocks

connections'' :: PositionedBlock -> [((Double, Double), (Double, Double))]
connections'' (PositionedFork _i _c l r _gCId x y maxX minY) =
  let lc = case l of
        [] -> [((x, y), (x, minY - defaultBoundingBoxHeight * 0.5))]
        bs@(b : _) ->
          let (lx, ly, _lmaxX, _lMinY) = getPosition b
           in case last bs of
                (PositionedFork _i _c _l _r _gCId _x _y _maxX _minY) -> [((x, y), (lx, ly))]
                lastB ->
                  let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                   in [((x, y), (lx, ly)), ((lastx, lasty), (x, minY))]
      rc = case r of
        [] -> [((x, y), (maxX - defaultBoundingBoxWidth * 0.5, y)), ((maxX - defaultBoundingBoxWidth * 0.5, y), (maxX - defaultBoundingBoxWidth * 0.5, minY))]
        bs@(b : _) ->
          let (rx, ry, _rmaxX, _rMinY) = getPosition b
           in case last bs of
                (PositionedFork _i _c _l _r _gCId _x _y _maxX _minY) -> [((x, y), (rx, y)), ((rx, y), (rx, ry))]
                lastB ->
                  let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                   in [((x, y), (rx, y)), ((rx, y), (rx, ry)), ((lastx, lasty), (lastx, minY))]
      branchConnection = ((maxX - defaultBoundingBoxWidth * 0.5, minY - 0.2), (x, minY)) -- TODO: overlapping connections
   in branchConnection : (lc <> rc <> connections' l <> connections' r)
connections'' _ = []

-- let position@(x, y, maxX, minY) = getPosition pB
--  in [((x, y), (x + 0.3, y - 0.5))]

connections' :: [PositionedBlock] -> [((Double, Double), (Double, Double))]
connections' [] = []
connections' [pB] = connections'' pB
connections' (pB1 : pB2 : pBs) =
  case pB1 of
    (PositionedFork _i _c l _r _gCId x1 y1 maxX1 minY1) ->
      let position2@(x2, y2, maxX2, minY2) = getPosition pB2
          lConnection = case l of
            [] -> []
            _ -> [((x1, minY1), (x2, y2))]
       in lConnection <> connections'' pB1 <> connections' (pB2 : pBs)
    _ ->
      let position1@(x1, y1, maxX1, minY1) = getPosition pB1
          position2@(x2, y2, maxX2, minY2) = getPosition pB2
          connection = [((x1, y1), (x2, y2))]
          remainingConnections = connections' (pB2 : pBs)
       in connection <> remainingConnections

connections :: [[PositionedBlock]] -> [((Double, Double), (Double, Double))]
connections = foldr (\pBs accu -> connections' pBs <> accu) []
