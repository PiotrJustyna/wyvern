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

reposition'' :: PositionedBlock -> Double -> PositionedBlock
reposition'' f@(PositionedFork i c l r gCId fx fy fmaxX fminY) y =
  if (fy <= y)
    then (PositionedFork i c (reposition' l y) (reposition' r y) gCId fx (fy - 0.1) fmaxX (fminY - 0.1))
    else f
reposition'' b y =
  let position@(bx, by, _bmaxX, _bminY) = getPosition b
   in if (by <= y)
        then setPosition b bx (by - 0.1)
        else b

reposition' :: [PositionedBlock] -> Double -> [PositionedBlock]
reposition' bs y = foldr (\b accu -> (reposition'' b y) : accu) [] bs

reposition :: [[PositionedBlock]] -> Double -> [[PositionedBlock]]
reposition ss y = foldr (\s accu -> (reposition' s y) : accu) [] ss

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
        [] -> [((x, y), (maxX - defaultBoundingBoxWidth * 0.5, y)), ((maxX - defaultBoundingBoxWidth * 0.5, y), (maxX - defaultBoundingBoxWidth * 0.5, minY)), ((maxX - defaultBoundingBoxWidth * 0.5, minY), (x, minY))]
        bs@(b : _) ->
          let (rx, ry, _rmaxX, _rMinY) = getPosition b
           in case last bs of
                (PositionedFork _i _c _l _r _gCId fx fy _maxX _minY) -> [((x, y), (rx, y)), ((rx, y), (rx, ry)), ((rx, minY), (x, minY)), ((fx, fy), (fx, minY))]
                lastB ->
                  let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                   in [((x, y), (rx, y)), ((rx, y), (rx, ry)), ((lastx, lasty), (lastx, minY)), ((lastx, minY), (x, minY))]
   in (lc <> rc <> connections' l <> connections' r)
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
