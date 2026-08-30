module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, repositionShift)
import Data.Map (Map, adjust, empty, insert, keys, lookup)
import ID
import PositionedBlock

position'' :: Block -> Double -> Double -> PositionedBlock
position'' (Fork i c l r gCId) x y =
  let (positionedLeft, lMaxX, lMinY) = position' l x (y - defaultBoundingBoxHeight * 0.5)
      xR = lMaxX + defaultBoundingBoxWidth * 0.5
      (positionedRight, rMaxX, rMinY) = case r of
        [] -> position' r lMaxX (y - defaultBoundingBoxHeight * 0.5)
        _ -> position' r xR (y - defaultBoundingBoxHeight * 0.5)
   in (PositionedFork i c (Prelude.reverse positionedLeft) (Prelude.reverse positionedRight) gCId x xR y rMaxX lMinY rMinY)
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
reposition'' b@(PositionedFork i c l r gCId x xR y maxX minYL minYR) thresholdDepth numberOfShifts =
  let (l', lAnyRepositioned) = reposition' l thresholdDepth numberOfShifts
      (r', rAnyRepositioned) = reposition' r thresholdDepth numberOfShifts
      anyBranchRepositioned = lAnyRepositioned || rAnyRepositioned
      shift = repositionShift * (fromIntegral numberOfShifts)
   in if (y <= thresholdDepth)
        then (PositionedFork i c l' r' gCId x xR (y - shift) maxX (minYL - shift) (minYR - shift), True)
        else (PositionedFork i c l' r' gCId x xR y maxX (minYL - shift) (minYR - shift), anyBranchRepositioned)
reposition'' b@(PositionedStartTerminator x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedStartTerminator x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedEndTerminator x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedEndTerminator x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedAction i c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedAction i c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedHeadline i c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedHeadline i c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
reposition'' b@(PositionedAddress i c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedAddress i c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)

repositionX'' :: PositionedBlock -> Double -> Int -> (PositionedBlock, Bool)
repositionX'' b@(PositionedFork i c l r gCId x xR y maxX minYL minYR) thresholdWidth numberOfShifts =
  let (l', lAnyRepositioned) = repositionX' l thresholdWidth numberOfShifts
      (r', rAnyRepositioned) = repositionX' r thresholdWidth numberOfShifts
      anyBranchRepositioned = lAnyRepositioned || rAnyRepositioned
      shift = repositionShift * (fromIntegral numberOfShifts)
   in if (x >= thresholdWidth)
        then (PositionedFork i c l' r' gCId (x + shift) (xR + shift) y (maxX + shift) minYL minYR, True)
        else (PositionedFork i c l' r' gCId x xR y (if anyBranchRepositioned then (maxX + shift) else maxX) minYL minYR, anyBranchRepositioned)
repositionX'' b@(PositionedStartTerminator x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedStartTerminator (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedEndTerminator x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedEndTerminator (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedAction i c x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedAction i c (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedHeadline i c x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedHeadline i c (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedAddress i c x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedAddress i c (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)

reposition' :: [PositionedBlock] -> Double -> Int -> ([PositionedBlock], Bool)
reposition' bs y numberOfShifts =
  foldr
    ( \b (accuRepositionedBlocks, accuAnyRepositioned) ->
        let (repositionedBlock, isRepositoned) = (reposition'' b y numberOfShifts)
         in (repositionedBlock : accuRepositionedBlocks, isRepositoned || accuAnyRepositioned)
    )
    ([], False)
    bs

repositionX' :: [PositionedBlock] -> Double -> Int -> ([PositionedBlock], Bool)
repositionX' bs x numberOfShifts =
  foldr
    ( \b (accuRepositionedBlocks, accuAnyRepositioned) ->
        let (repositionedBlock, isRepositoned) = (repositionX'' b x numberOfShifts)
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

repositionX :: [[PositionedBlock]] -> Double -> Int -> ([[PositionedBlock]], Bool)
repositionX ss x numberOfShifts =
  foldr
    ( \s (accuRepositionedSkewers, accuAnyRepositioned) ->
        let (repositionedSkewer, isRepositioned) = (repositionX' s x numberOfShifts)
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
        then
          -- 2026-08-24 PJ:
          -- ##############
          -- Predictable shape of a gamma connection.
          -- [((x, y), (x + defaultBoundingBoxWidth, y))]
          [((x, y), (newMaxX, y)), ((newMaxX, y), (newMaxX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5)), ((newMaxX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5), (dX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5))]
        else [((x, y), (dX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5))]

buildGammaConnection :: ID -> Map ID (Double, Double, Double, Double, Double, Double) -> Double -> Double -> Double -> ([((Double, Double), (Double, Double))], Map ID (Double, Double, Double, Double, Double, Double))
buildGammaConnection gCId destinations x y maxX =
  case Data.Map.lookup gCId destinations of
    Nothing -> error $ "gamma connection id \"" <> show gCId <> "\" does not exist in the collection of block identifiers: " <> show destinations
    (Just destination) -> (buildGammaConnection' x y maxX destination, Data.Map.adjust (\(vX, vY, vMaxX, vMinY, vGammaShiftX, vGammaShiftY) -> (vX, vY, vMaxX, vMinY, vGammaShiftX + repositionShift, vGammaShiftY + repositionShift)) gCId destinations)

connectionsV2'' :: PositionedBlock -> [((Double, Double), (Double, Double))]
connectionsV2'' (PositionedFork _i _c l r gCId x xR y maxX minYL minYR) =
  let minY = min minYL minYR
      lc = case l of
        [] -> [((x, y), (x, minY - defaultBoundingBoxHeight * 0.25))]
        bs@(b : _) ->
          let (lx, ly, _lmaxX, _lMinY) = getPosition b
           in case last bs of
                (PositionedFork _i _c _l _r _gCId _x _xR _y _maxX _minYL _minYR) -> [((x, y), (lx, ly))]
                lastB ->
                  let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                   in [((x, y), (lx, ly)), ((lastx, lasty), (x, minY))]
      rc = case r of
        [] -> case gCId of
          Nothing -> [((x, y), (maxX, y)), ((maxX, y), (maxX, minY)), ((maxX, minY), (x, minY))]
          _ -> []
        bs@(b : _) ->
          let (rx, ry, _rmaxX, _rMinY) = getPosition b
              leadingRc = [((x, y), (rx, y)), ((rx, y), (rx, ry))]
           in case last bs of
                (PositionedFork _i _c _l _r _gCId fx _fxR fy _maxX _minY minYR) -> case gCId of
                  Nothing -> leadingRc <> [((rx, minY), (x, minY)), ((fx, fy), (fx, minYR))]
                  _ -> leadingRc
                lastB ->
                  let (lastx, lasty, _lastmaxX, _lastMinY) = getPosition lastB
                   in case gCId of
                        Nothing -> leadingRc <> [((lastx, lasty), (lastx, minY)), ((lastx, minY), (x, minY))]
                        _ -> leadingRc <> [((lastx, lasty), (lastx, minYR))]
      lc' = connectionsV2' l
      -- 2026-07-29 PJ:
      -- ==============
      -- The section below (lc'') adds an extra line connecting the left branch with the end of the fork.
      -- We need that extra line when the right branch is longer than the left one.
      -- Without it, in such scenarios, there would be a gap between the end of the fork and the last left branch's block.
      lc'' = case lc' of
        [] -> []
        _ ->
          let (lLPX, lLPY) = snd $ last lc'
           in if lLPY > minY + defaultBoundingBoxHeight then [((lLPX, lLPY - defaultBoundingBoxHeight * 0.5), (x, minY))] else []
      rc' = connectionsV2' r
      rc'' = case rc' of
        [] -> []
        _ ->
          let (rLPX, rLPY) = snd $ last rc'
           in if rLPY > minY + defaultBoundingBoxHeight then [((rLPX, rLPY - defaultBoundingBoxHeight * 0.5), (rLPX, minY))] else []
   in lc <> rc <> lc' <> rc' <> lc'' <> rc''
connectionsV2'' _ = []

connectionsV2' :: [PositionedBlock] -> [((Double, Double), (Double, Double))]
connectionsV2' [] = []
connectionsV2' [pB] = connectionsV2'' pB
connectionsV2' (pB1 : pB2 : pBs) =
  case pB1 of
    (PositionedFork _i _c l _r _gCId x1 _x1R y1 maxX1 minYL1 minYR1) ->
      let minY1 = min minYL1 minYR1
          position2@(x2, y2, maxX2, minY2) = getPosition pB2
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

barebonesGamma'' :: PositionedBlock -> [((Double, Double), ID, Double)]
barebonesGamma'' (PositionedFork _i _c l r gCId x xR y maxX minYL minYR) =
  let lGamma = barebonesGamma' l
      rGamma = barebonesGamma' r
      gamma = case gCId of
        Nothing -> []
        (Just gCId') -> case r of
          [] -> [((x, y), gCId', maxX)]
          _ -> [((xR, minYR), gCId', maxX)]
   in rGamma <> lGamma <> gamma
barebonesGamma'' _ = []

barebonesGamma' :: [PositionedBlock] -> [((Double, Double), ID, Double)]
barebonesGamma' = foldr (\pB accuGamma -> accuGamma <> barebonesGamma'' pB) []

barebonesGamma :: [[PositionedBlock]] -> [((Double, Double), ID, Double)]
barebonesGamma = foldr (\pBs accuGamma -> accuGamma <> barebonesGamma' pBs) []

-- 2026-08-25 PJ:
-- ==============
-- TODO: simplify if V3 is the right approach.
repositionBasedOnGamma :: [[PositionedBlock]] -> [(Double, Int)] -> [[PositionedBlock]]
repositionBasedOnGamma positionedBlocks repositionInstructions = foldr (\(thresholdDepth, numberOfShifts) accu -> fst $ reposition accu thresholdDepth numberOfShifts) positionedBlocks repositionInstructions

-- 2026-08-25 PJ:
-- ==============
-- TODO: simplify if V3 is the right approach.
repositionBasedOnGammaX :: [[PositionedBlock]] -> [(Double, Int)] -> [[PositionedBlock]]
repositionBasedOnGammaX positionedBlocks repositionInstructions = foldr (\(thresholdWidth, numberOfShifts) accu -> fst $ repositionX accu thresholdWidth numberOfShifts) positionedBlocks repositionInstructions

repositionOriginsBasedOnGamma :: Double -> Double -> [((Double, Double), ID, Double)] -> [((Double, Double), ID, Double)]
repositionOriginsBasedOnGamma _repositionInstructionsX _repositionInstructionsY [] = []
repositionOriginsBasedOnGamma repositionInstructionsX repositionInstructionsY (o@((originX, originY), _gCId, _maxXOrigin) : os) =
  ((if originX >= repositionInstructionsX then originX + repositionShift else originX, if originY <= repositionInstructionsY then originY - repositionShift else originY), _gCId, _maxXOrigin) : repositionOriginsBasedOnGamma repositionInstructionsX repositionInstructionsY os
