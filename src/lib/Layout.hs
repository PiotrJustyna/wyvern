module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, repositionShift)
import Data.Map (Map, adjust, empty, insert, keys, lookup)
import ID
import PositionedBlock

position'' :: Block -> Double -> Double -> PositionedBlock
position'' (Fork i c l r gCId) x y =
  let (positionedLeft, lMaxX, lMinY) = position' l x (y - defaultBoundingBoxHeight * 0.5)
      (positionedRight, rMaxX, rMinY) = case r of
        [] -> position' r lMaxX (y - defaultBoundingBoxHeight * 0.5)
        _ -> position' r (lMaxX + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5)
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

reposition'' :: PositionedBlock -> Double -> Int -> (PositionedBlock, Bool)
reposition'' b@(PositionedFork i c l r gCId x y maxX minY) thresholdDepth numberOfShifts =
  let (l', lAnyRepositioned) = reposition' l thresholdDepth numberOfShifts
      (r', rAnyRepositioned) = reposition' r thresholdDepth numberOfShifts
      anyBranchRepositioned = lAnyRepositioned || rAnyRepositioned
   in if (y <= thresholdDepth)
        then (PositionedFork i c l' r' gCId x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True)
        else (PositionedFork i c l' r' gCId x y maxX (if anyBranchRepositioned then minY - repositionShift else minY), anyBranchRepositioned)
reposition'' b@(PositionedStartTerminator x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedStartTerminator x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedEndTerminator x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedEndTerminator x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedAction i c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedAction i c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedHeadline i c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedHeadline i c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedAddress i c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedAddress i c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)

reposition' :: [PositionedBlock] -> Double -> Int -> ([PositionedBlock], Bool)
reposition' bs y numberOfShifts =
  foldr
    ( \b (accuRepositionedBlocks, accuAnyRepositioned) ->
        let (repositionedBlock, isRepositoned) = (reposition'' b y numberOfShifts)
         in (repositionedBlock : accuRepositionedBlocks, isRepositoned || accuAnyRepositioned)
    )
    ([], False)
    bs

reposition :: [[PositionedBlock]] -> Double -> Int -> ([[PositionedBlock]], Bool)
reposition ss y numberOfShifts =
  foldr
    ( \s (accuRepositionedSkewers, accuAnyRepositioned) ->
        let (repositionedSkewer, isRepositioned) = (reposition' s y numberOfShifts)
         in (repositionedSkewer : accuRepositionedSkewers, isRepositioned || accuAnyRepositioned)
    )
    ([], False)
    ss

buildGammaConnection' :: Double -> Double -> Double -> (Double, Double, Double, Double, Double, Double) -> [((Double, Double), (Double, Double))]
buildGammaConnection' x y oMaxX (dX, dY, dMaxX, _dMinY, dGammaShiftX, dGammaShiftY) =
  let newMaxX = max oMaxX (dMaxX + dGammaShiftX)
   in if dX <= x && dY >= y
        -- TODO 1: move the origin point of a block to the upper left corner of a block
        -- TODO 2: push the whole destination down and increase its width
        then [((x, y), (newMaxX, y)), ((newMaxX, y), (newMaxX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5)), ((newMaxX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5), (dX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5))]
        else [((x, y), (oMaxX - defaultBoundingBoxWidth * 0.5, y)), ((oMaxX - defaultBoundingBoxWidth * 0.5, y), (oMaxX - defaultBoundingBoxWidth * 0.5 + 1.0, y + 1.0)), ((oMaxX - defaultBoundingBoxWidth * 0.5 + 1.0, y + 1.0), (dX, dY))]

buildGammaConnection :: ID -> Map ID (Double, Double, Double, Double, Double, Double) -> Double -> Double -> Double -> ([((Double, Double), (Double, Double))], Map ID (Double, Double, Double, Double, Double, Double))
buildGammaConnection gCId destinations x y maxX =
  case Data.Map.lookup gCId destinations of
    Nothing -> error $ "gamma connection id \"" <> show gCId <> "\" does not exist in the collection of block identifiers: " <> show destinations
    (Just destination) -> (buildGammaConnection' x y maxX destination, Data.Map.adjust (\(vX, vY, vMaxX, vMinY, vGammaShiftX, vGammaShiftY) -> (vX, vY, vMaxX, vMinY, vGammaShiftX + 0.1, vGammaShiftY + 0.1)) gCId destinations)

connections'' :: PositionedBlock -> Map ID (Double, Double, Double, Double, Double, Double) -> ([((Double, Double), (Double, Double))], Map ID (Double, Double, Double, Double, Double, Double))
connections'' (PositionedFork _i _c l r gCId x y maxX minY) destinations =
  let (lc, lDestinations) = case l of
        [] -> ([((x, y), (x, minY - defaultBoundingBoxHeight * 0.25))], destinations)
        bs@(b : _) ->
          let (lx, ly, _lmaxX, _lMinY) = getPosition b
           in case last bs of
                (PositionedFork _i _c _l _r _gCId _x _y _maxX _minY) -> ([((x, y), (lx, ly))], destinations)
                lastB ->
                  let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                   in ([((x, y), (lx, ly)), ((lastx, lasty), (x, minY - defaultBoundingBoxHeight * 0.25))], destinations)
      (rc, rDestinations) = case gCId of
        Nothing -> case r of
          [] -> ([((x, y), (maxX - defaultBoundingBoxWidth * 0.5, y)), ((maxX - defaultBoundingBoxWidth * 0.5, y), (maxX - defaultBoundingBoxWidth * 0.5, minY)), ((maxX - defaultBoundingBoxWidth * 0.5, minY), (x, minY))], lDestinations)
          bs@(b : _) ->
            let (rx, ry, _rmaxX, _rMinY) = getPosition b
             in case last bs of
                  (PositionedFork _i _c _l _r _gCId fx fy _maxX _minY) -> ([((x, y), (rx, y)), ((rx, y), (rx, ry)), ((rx, minY), (x, minY)), ((fx, fy), (fx, minY))], lDestinations) -- TODO: ((rx, minY), (x, minY)) a mistake? ((fx, fy), (fx, minY)) - also a mistake and can lead to clashes
                  lastB ->
                    let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                     in ([((x, y), (rx, y)), ((rx, y), (rx, ry)), ((lastx, lasty), (lastx, minY)), ((lastx, minY), (x, minY))], lDestinations)
        (Just gCId') -> case r of
          [] -> buildGammaConnection gCId' lDestinations x y maxX
          bs@(b : _) ->
            let (rx, ry, _rmaxX, _rMinY) = getPosition b
             in case last bs of
                  (PositionedFork _i _c _l _r _gCId fx fy fmaxX fminY) ->
                    let (gammaConnections, lDestinations') = buildGammaConnection gCId' lDestinations fx fminY fmaxX
                     in ([((x, y), (rx, y)), ((rx, y), (rx, ry))] <> gammaConnections, lDestinations')
                  lastB ->
                    let (lastx, lasty, lastmaxX, _lastMinY) = getPosition lastB
                        lasty' = lasty - defaultBoundingBoxHeight * 0.5
                        (gammaConnections, lDestinations') = buildGammaConnection gCId' lDestinations lastx lasty' lastmaxX
                     in ([((x, y), (rx, y)), ((rx, y), (rx, ry)), ((lastx, lasty), (lastx, lasty'))] <> gammaConnections, lDestinations') -- TODO: there are two connections here: the leading one and the gamma one
      (lc', lDestinations') = connections' l rDestinations
      -- 2026-07-29 PJ:
      -- ==============
      -- The section below (lc'') adds an extra line connecting the left branch with the end of the fork.
      -- We need that extra line when the right branch is longer than the left one.
      -- Without it, in such scenarios, there would be a gap between the end of the fork and the last left branch's block.
      lastLeftPosition@(lLPX, lLPY) = snd $ last lc'
      lc'' = lc' <> (if lLPY > minY + defaultBoundingBoxHeight then [((lLPX, lLPY - defaultBoundingBoxHeight), (x, minY))] else [])
      (rc', rDestinations') = connections' r lDestinations'
   in (lc <> rc <> lc' <> lc'' <> rc', rDestinations')
connections'' _ destinations = ([], destinations)

connections' :: [PositionedBlock] -> Map ID (Double, Double, Double, Double, Double, Double) -> ([((Double, Double), (Double, Double))], Map ID (Double, Double, Double, Double, Double, Double))
connections' [] destinations = ([], destinations)
connections' [pB] destinations = connections'' pB destinations
connections' (pB1 : pB2 : pBs) destinations =
  case pB1 of
    (PositionedFork _i _c l _r _gCId x1 y1 maxX1 minY1) ->
      let position2@(x2, y2, maxX2, minY2) = getPosition pB2
          lConnection = case l of
            [] -> []
            _ -> [((x1, minY1), (x2, y2))]
          (firstConnections, firstDestinations) = connections'' pB1 destinations
          (remainingConnections, remainingDestinations) = connections' (pB2 : pBs) firstDestinations
       in (lConnection <> firstConnections <> remainingConnections, remainingDestinations)
    _ ->
      let position1@(x1, y1, maxX1, minY1) = getPosition pB1
          position2@(x2, y2, maxX2, minY2) = getPosition pB2
          connection = [((x1, y1), (x2, y2))]
          (remainingConnections, remainingDestinations) = connections' (pB2 : pBs) destinations
       in (connection <> remainingConnections, remainingDestinations)

connections :: [[PositionedBlock]] -> Map ID (Double, Double, Double, Double, Double, Double) -> [((Double, Double), (Double, Double))]
connections positionedBlocks destinations =
  fst $
    foldr
      ( \pBs (accuConnections, accuDestinations) ->
          let (newConnections, newDestinations) = connections' pBs destinations
           in (newConnections <> accuConnections, newDestinations)
      )
      ([], destinations)
      positionedBlocks

connectionsV2'' :: PositionedBlock -> [((Double, Double), (Double, Double))]
connectionsV2'' (PositionedFork _i _c l r gCId x y maxX minY) =
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
      lc' = connectionsV2' l
      -- 2026-07-29 PJ:
      -- ==============
      -- The section below (lc'') adds an extra line connecting the left branch with the end of the fork.
      -- We need that extra line when the right branch is longer than the left one.
      -- Without it, in such scenarios, there would be a gap between the end of the fork and the last left branch's block.
      lastLeftPosition@(lLPX, lLPY) = snd $ last lc'
      lc'' = lc' <> (if lLPY > minY + defaultBoundingBoxHeight then [((lLPX, lLPY - defaultBoundingBoxHeight), (x, minY))] else [])
      rc' = connectionsV2' r
   in lc <> rc <> lc' <> lc'' <> rc'
connectionsV2'' _ = []

connectionsV2' :: [PositionedBlock] -> [((Double, Double), (Double, Double))]
connectionsV2' [] = []
connectionsV2' [pB] = connectionsV2'' pB
connectionsV2' (pB1 : pB2 : pBs) =
  case pB1 of
    (PositionedFork _i _c l _r _gCId x1 y1 maxX1 minY1) ->
      let position2@(x2, y2, maxX2, minY2) = getPosition pB2
          lConnection = case l of
            [] -> []
            _ -> [((x1, minY1), (x2, y2))]
          firstConnections = connectionsV2'' pB1
          remainingConnections = connectionsV2' (pB2 : pBs)
       in lConnection <> firstConnections <> remainingConnections
    _ ->
      let position1@(x1, y1, maxX1, minY1) = getPosition pB1
          position2@(x2, y2, maxX2, minY2) = getPosition pB2
          connection = [((x1, y1), (x2, y2))]
          remainingConnections = connectionsV2' (pB2 : pBs)
       in connection <> remainingConnections

connectionsV2 :: [[PositionedBlock]] -> [((Double, Double), (Double, Double))]
connectionsV2 = foldr (\pBs accuConnections -> accuConnections <> connectionsV2' pBs) []

barebonesGamma'' :: PositionedBlock -> [((Double, Double), ID)]
barebonesGamma'' (PositionedFork _i _c l r gCId x y maxX minY) =
  let lGamma = barebonesGamma' l
      rGamma = barebonesGamma' r
      gamma = case gCId of
        Nothing -> []
        (Just gCId') -> case r of
          [] -> [((x, y), gCId')]
          _ -> [((maxX, minY), gCId')]
   in gamma <> lGamma <> rGamma
barebonesGamma'' _ = []

barebonesGamma' :: [PositionedBlock] -> [((Double, Double), ID)]
barebonesGamma' = foldr (\pB accuGamma -> accuGamma <> barebonesGamma'' pB) []

barebonesGamma :: [[PositionedBlock]] -> [((Double, Double), ID)]
barebonesGamma = foldr (\pBs accuGamma -> accuGamma <> barebonesGamma' pBs) []

repositionBasedOnGamma :: [[PositionedBlock]] -> [(Double, Int)] -> [[PositionedBlock]]
repositionBasedOnGamma positionedBlocks repositionInstructions = foldr (\(thresholdDepth, numberOfShifts) accu -> fst $ reposition accu thresholdDepth numberOfShifts) positionedBlocks repositionInstructions
