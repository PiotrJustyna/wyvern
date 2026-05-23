module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, repositionShift)
import PositionedBlock

position'' :: Block -> Double -> Double -> PositionedBlock
position'' (Fork i c l r gCId) x y =
  let (positionedLeft, lMaxX, lMinY) = position' l x (y - defaultBoundingBoxHeight * 0.5)
      (positionedRight, rMaxX, rMinY) = position' r (lMaxX + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)
   in (PositionedFork i c (Prelude.reverse positionedLeft) (Prelude.reverse positionedRight) gCId x y rMaxX ((min lMinY rMinY) - defaultBoundingBoxHeight * 0.25))
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

reposition'' :: PositionedBlock -> Double -> (PositionedBlock, Bool)
reposition'' b@(PositionedFork i c l r gCId x y maxX minY) newY =
  let (l', lAnyRepositioned) = reposition' l newY
      (r', rAnyRepositioned) = reposition' r newY
      anyBranchRepositioned = lAnyRepositioned || rAnyRepositioned
   in if (y <= newY)
        then (PositionedFork i c l' r' gCId x (y - repositionShift) maxX (minY - repositionShift), True)
        else (PositionedFork i c l' r' gCId x y maxX (if anyBranchRepositioned then minY - repositionShift else minY), anyBranchRepositioned)
reposition'' b@(PositionedStartTerminator x y maxX minY) newY = if (y <= newY) then (PositionedStartTerminator x (y - repositionShift) maxX (minY - repositionShift), True) else (b, False)
reposition'' b@(PositionedEndTerminator x y maxX minY) newY = if (y <= newY) then (PositionedEndTerminator x (y - repositionShift) maxX (minY - repositionShift), True) else (b, False)
reposition'' b@(PositionedAction i c x y maxX minY) newY = if (y <= newY) then (PositionedAction i c x (y - repositionShift) maxX (minY - repositionShift), True) else (b, False)
reposition'' b@(PositionedHeadline i c x y maxX minY) newY = if (y <= newY) then (PositionedHeadline i c x (y - repositionShift) maxX (minY - repositionShift), True) else (b, False)
reposition'' b@(PositionedAddress i c x y maxX minY) newY = if (y <= newY) then (PositionedAddress i c x (y - repositionShift) maxX (minY - repositionShift), True) else (b, False)

reposition' :: [PositionedBlock] -> Double -> ([PositionedBlock], Bool)
reposition' bs y =
  foldr
    ( \b (accuRepositionedBlocks, accuAnyRepositioned) ->
        let (repositionedBlock, isRepositoned) = (reposition'' b y)
         in (repositionedBlock : accuRepositionedBlocks, isRepositoned || accuAnyRepositioned)
    )
    ([], False)
    bs

reposition :: [[PositionedBlock]] -> Double -> ([[PositionedBlock]], Bool)
reposition ss y =
  foldr
    ( \s (accuRepositionedSkewers, accuAnyRepositioned) ->
        let (repositionedSkewer, isRepositioned) = (reposition' s y)
         in (repositionedSkewer : accuRepositionedSkewers, isRepositioned || accuAnyRepositioned)
    )
    ([], False)
    ss

connections'' :: PositionedBlock -> [((Double, Double), (Double, Double))]
connections'' (PositionedFork _i _c l r _gCId x y maxX minY) =
  let lc = case l of
        [] -> [((x, y), (x, minY - defaultBoundingBoxHeight * 0.25))]
        bs@(b : _) ->
          let (lx, ly, _lmaxX, _lMinY) = getPosition b
           in case last bs of
                (PositionedFork _i _c _l _r _gCId _x _y _maxX _minY) -> [((x, y), (lx, ly))]
                lastB ->
                  let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                   in [((x, y), (lx, ly)), ((lastx, lasty), (x, minY - defaultBoundingBoxHeight * 0.25))]
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
