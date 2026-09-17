module Layout where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, repositionShift)
import Data.Map (Map, adjust, empty, insert, keys, lookup)
import ID
import PositionedBlock

position'' :: Block -> Int -> Double -> Double -> (PositionedBlock, Int)
position'' (Fork i c l r gCId) pId x y =
  let (positionedLeft, lPId, lMaxX, lMinY) = position' l (pId + 1) x (y - defaultBoundingBoxHeight * 0.5)
      xR = lMaxX + defaultBoundingBoxWidth * 0.5
      (positionedRight, rPId, rMaxX, rMinY) = case r of
        [] -> position' r lPId lMaxX (y - defaultBoundingBoxHeight * 0.5)
        _ -> position' r lPId xR (y - defaultBoundingBoxHeight * 0.5)
   in (PositionedFork i pId c (Prelude.reverse positionedLeft) (Prelude.reverse positionedRight) gCId x xR y rMaxX lMinY rMinY (-1.0) rMinY, rPId)
position'' StartTerminator pId x y = (PositionedStartTerminator pId x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5), pId)
position'' (Action i c) pId x y = (PositionedAction i pId c x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5), pId)
position'' (Headline i c) pId x y = (PositionedHeadline i pId c x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5), pId)
position'' (Address i c) pId x y = (PositionedAddress i pId c x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5), pId)
position'' EndTerminator pId x y = (PositionedEndTerminator pId x y (x + defaultBoundingBoxWidth * 0.5) (y - defaultBoundingBoxHeight * 0.5), pId)

position' :: [Block] -> Int -> Double -> Double -> ([PositionedBlock], Int, Double, Double) -- TODO: should this also return lRY?
position' blocks pId x y =
  let (_finalX, _finalY, finalPositionedBlocks, finalMaxX, finalMinY, finalPId) =
        foldl
          ( \(accuX, accuY, accuPositionedBlocks, accuMaxX, accuMinY, accuPId) b ->
              let (positionedBlock, accuPId') = position'' b accuPId accuX (accuY - defaultBoundingBoxHeight * 0.5)
                  (_x, _y, maxX, minY) = getPosition positionedBlock
               in ( accuX,
                    minY,
                    positionedBlock : accuPositionedBlocks,
                    max maxX accuMaxX,
                    min minY accuMinY,
                    accuPId' + 1
                  )
          )
          (x, y, [], x, y, pId)
          blocks
   in (finalPositionedBlocks, finalPId, finalMaxX, finalMinY)

position :: [[Block]] -> Int -> Double -> Double -> [[PositionedBlock]]
position skewers pId x y =
  let (_finalMaxX, finalPositionedBlocks, _finalPId) =
        foldl
          ( \(accuMaxX, accuPositionedBlocks, accuPId) skewer ->
              let (positionedSkewer, accuPId', maxX, _minY) = position' skewer accuPId accuMaxX y
               in (maxX + defaultBoundingBoxWidth * 0.5, (Prelude.reverse positionedSkewer) : accuPositionedBlocks, accuPId')
          )
          (x, [], pId)
          skewers
   in finalPositionedBlocks

repositionTopY'' :: PositionedBlock -> Double -> Int -> (PositionedBlock, Bool)
repositionTopY'' b@(PositionedFork i pId c l r gCId x xR y maxX minYL minYR _lLY lRY) thresholdDepth numberOfShifts =
  let (l', lAnyRepositioned) = repositionTopY' l thresholdDepth numberOfShifts
      (r', rAnyRepositioned) = repositionTopY' r thresholdDepth numberOfShifts
      anyBranchRepositioned = lAnyRepositioned || rAnyRepositioned
      shift = repositionShift * (fromIntegral numberOfShifts)
   in if (y <= thresholdDepth)
        then (PositionedFork i pId c l' r' gCId x xR (y - shift) maxX (minYL - shift) (minYR - shift) _lLY (lRY - shift), True)
        else (PositionedFork i pId c l' r' gCId x xR y maxX (minYL - shift) (minYR - shift) _lLY (lRY - shift), anyBranchRepositioned) -- TODO: I am not entirely sure this is correct, shouldn't the shifts depend on "anyBranchRepositioned"?
repositionTopY'' b@(PositionedStartTerminator pId x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedStartTerminator pId x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionTopY'' b@(PositionedEndTerminator pId x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedEndTerminator pId x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionTopY'' b@(PositionedAction i pId c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedAction i pId c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionTopY'' b@(PositionedHeadline i pId c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedHeadline i pId c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionTopY'' b@(PositionedAddress i pId c x y maxX minY) thresholdDepth numberOfShifts = if (y <= thresholdDepth) then (PositionedAddress i pId c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)

repositionBottomY'' :: PositionedBlock -> Double -> Int -> (PositionedBlock, Bool)
repositionBottomY'' b@(PositionedFork i pId c l r gCId x xR y maxX minYL minYR _lLY lRY) thresholdDepth numberOfShifts =
  let (l', lAnyRepositioned) = repositionBottomY' l thresholdDepth numberOfShifts
      (r', rAnyRepositioned) = repositionBottomY' r thresholdDepth numberOfShifts
      anyBranchRepositioned = lAnyRepositioned || rAnyRepositioned
      shift = repositionShift * (fromIntegral numberOfShifts)
   in if (y < thresholdDepth)
        then (PositionedFork i pId c l' r' gCId x xR (y - shift) maxX (minYL - shift) (minYR - shift) _lLY (lRY - shift), True)
        else (PositionedFork i pId c l' r' gCId x xR y maxX (minYL - if anyBranchRepositioned || minYL <= thresholdDepth then shift else 0.0) (minYR - if anyBranchRepositioned || minYR <= thresholdDepth then shift else 0.0) _lLY (lRY - if anyBranchRepositioned || lRY <= thresholdDepth then shift else 0.0), anyBranchRepositioned)
repositionBottomY'' b@(PositionedStartTerminator pId x y maxX minY) thresholdDepth numberOfShifts = if (y < thresholdDepth) then (PositionedStartTerminator pId x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionBottomY'' b@(PositionedEndTerminator pId x y maxX minY) thresholdDepth numberOfShifts = if (y < thresholdDepth) then (PositionedEndTerminator pId x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionBottomY'' b@(PositionedAction i pId c x y maxX minY) thresholdDepth numberOfShifts = if (y < thresholdDepth) then (PositionedAction i pId c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionBottomY'' b@(PositionedHeadline i pId c x y maxX minY) thresholdDepth numberOfShifts = if (y < thresholdDepth) then (PositionedHeadline i pId c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)
repositionBottomY'' b@(PositionedAddress i pId c x y maxX minY) thresholdDepth numberOfShifts = if (y < thresholdDepth) then (PositionedAddress i pId c x (y - repositionShift * (fromIntegral numberOfShifts)) maxX (minY - repositionShift * (fromIntegral numberOfShifts)), True) else (b, False)

repositionX'' :: PositionedBlock -> Double -> Int -> (PositionedBlock, Bool)
repositionX'' b@(PositionedFork i pId c l r gCId x xR y maxX minYL minYR lLY lRY) thresholdWidth numberOfShifts =
  let (l', lAnyRepositioned) = repositionX' l thresholdWidth numberOfShifts
      (r', rAnyRepositioned) = repositionX' r thresholdWidth numberOfShifts
      anyBranchRepositioned = lAnyRepositioned || rAnyRepositioned
      shift = repositionShift * (fromIntegral numberOfShifts)
   in if (x >= thresholdWidth)
        then (PositionedFork i pId c l' r' gCId (x + shift) (xR + shift) y (maxX + shift) minYL minYR lLY lRY, True)
        else (PositionedFork i pId c l' r' gCId x (if anyBranchRepositioned then (xR + shift) else xR) y (if anyBranchRepositioned then (maxX + shift) else maxX) minYL minYR lLY lRY, anyBranchRepositioned)
repositionX'' b@(PositionedStartTerminator pId x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedStartTerminator pId (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedEndTerminator pId x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedEndTerminator pId (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedAction i pId c x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedAction i pId c (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedHeadline i pId c x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedHeadline i pId c (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)
repositionX'' b@(PositionedAddress i pId c x y maxX minY) thresholdWidth numberOfShifts = if (x >= thresholdWidth) then (PositionedAddress i pId c (x + repositionShift * (fromIntegral numberOfShifts)) y (maxX + repositionShift * (fromIntegral numberOfShifts)) minY, True) else (b, False)

repositionTopY' :: [PositionedBlock] -> Double -> Int -> ([PositionedBlock], Bool)
repositionTopY' bs y numberOfShifts =
  foldr
    ( \b (accuRepositionedBlocks, accuAnyRepositioned) ->
        let (repositionedBlock, isRepositoned) = (repositionTopY'' b y numberOfShifts)
         in (repositionedBlock : accuRepositionedBlocks, isRepositoned || accuAnyRepositioned)
    )
    ([], False)
    bs

repositionBottomY' :: [PositionedBlock] -> Double -> Int -> ([PositionedBlock], Bool)
repositionBottomY' bs y numberOfShifts =
  foldr
    ( \b (accuRepositionedBlocks, accuAnyRepositioned) ->
        let (repositionedBlock, isRepositoned) = (repositionBottomY'' b y numberOfShifts)
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

repositionX :: Double -> [[PositionedBlock]] -> [[PositionedBlock]]
repositionX x = foldr (\b accu -> (fst $ repositionX' b x 1) : accu) []

repositionTopY :: Double -> [[PositionedBlock]] -> [[PositionedBlock]]
repositionTopY y = foldr (\b accu -> (fst $ repositionTopY' b y 1) : accu) []

repositionBottomY :: Double -> [[PositionedBlock]] -> [[PositionedBlock]]
repositionBottomY y = foldr (\b accu -> (fst $ repositionBottomY' b y 1) : accu) []

anotherReposition :: [[PositionedBlock]] -> (Int, Double, Double) -> [[PositionedBlock]]
anotherReposition bss x =
  foldr
    (\bs accu -> (anotherReposition' bs x) : accu)
    []
    bss

anotherReposition' :: [PositionedBlock] -> (Int, Double, Double) -> [PositionedBlock]
anotherReposition' bs x =
  foldr
    (\b accu -> (anotherReposition'' b x) : accu)
    []
    bs

anotherReposition'' :: PositionedBlock -> (Int, Double, Double) -> PositionedBlock
anotherReposition'' b@(PositionedFork i pId c l r gCId x xR y maxX minYL minYR lLY lRY) origin@(pId', minYR', shift) =
  let l' = anotherReposition' l origin
      r' = anotherReposition' r origin
   in if (pId == pId')
        then PositionedFork i pId c l' r' gCId x xR y maxX minYL (minYR' - shift) lLY lRY
        else PositionedFork i pId c l' r' gCId x xR y maxX minYL minYR' lLY lRY
anotherReposition'' b _x = b

buildGammaConnection' ::
  Double ->
  Double ->
  Double ->
  Double ->
  (Double, Double, Double, Double, Double, Double) ->
  [((Double, Double), (Double, Double))]
buildGammaConnection' x y oMaxX oGammaShiftY (dX, dY, dMaxX, _dMinY, dGammaShiftX, dGammaShiftY) =
  let newMaxX = (max oMaxX dMaxX) + dGammaShiftX
   in if dX <= x && dY >= y
        -- TODO 1: move the origin point of a block to the upper left corner of a block
        -- TODO 2: push the whole destination down and increase its width
        then
          -- 2026-08-24 PJ:
          -- ##############
          -- Predictable shape of a gamma connection.
          -- [((x, y), (x + defaultBoundingBoxWidth, y))]
          [ ((x, y - oGammaShiftY), (newMaxX, y - oGammaShiftY)), -- bottom horizontal line
            ((newMaxX, y - oGammaShiftY), (newMaxX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5)), -- right vertical line
            ((newMaxX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5), (dX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5)) -- top horizontal line
          ]
        else [((x, y), (dX, dY + dGammaShiftY + defaultBoundingBoxHeight * 0.5))]

buildGammaConnection ::
  ID ->
  Map Double Double ->
  Map ID (Double, Double, Double, Double, Double, Double) ->
  Double ->
  Double ->
  Double ->
  ([((Double, Double), (Double, Double))], Map Double Double, Map ID (Double, Double, Double, Double, Double, Double))
buildGammaConnection gCId origins destinations x y maxX =
  case Data.Map.lookup gCId destinations of
    Nothing -> error $ "gamma connection id \"" <> show gCId <> "\" does not exist in the collection of block identifiers: " <> show destinations
    (Just destination) ->
      case Data.Map.lookup y origins of
        Nothing -> error $ "origin coordinate y \"" <> show y <> "\" does not exist in the collection of origins: " <> show origins
        (Just oGammaShiftY) ->
          ( buildGammaConnection' x y maxX (oGammaShiftY + repositionShift) destination,
            Data.Map.adjust (\vOGammaShiftY -> vOGammaShiftY + repositionShift) y origins,
            Data.Map.adjust (\(vX, vY, vMaxX, vMinY, vDGammaShiftX, vDGammaShiftY) -> (vX, vY, vMaxX, vMinY, vDGammaShiftX + repositionShift, vDGammaShiftY + repositionShift)) gCId destinations
          )

connectionsV2'' :: PositionedBlock -> [((Double, Double), (Double, Double))]
connectionsV2'' (PositionedFork _i _pId _c l r gCId x xR y maxX minYL minYR _lLY _lRY) =
  let minY = min minYL minYR
      lc = case l of
        [] -> [((x, y), (x, minY - defaultBoundingBoxHeight * 0.25))]
        bs@(b : _) ->
          let (lx, ly, _lmaxX, _lMinY) = getPosition b
           in case last bs of
                (PositionedFork _i _pId _c _l _r _gCId _x _xR _y _maxX _minYL _minYR _lLY _lRY) -> [((x, y), (lx, ly))]
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
                (PositionedFork _i _pId _c _l _r _gCId fx _fxR fy _maxX _minY minYR _lLY _lRY) -> case gCId of
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
        _ -> []
      -- case gCId of
      --   Nothing ->
      --     let (lLPX, lLPY) = snd $ last lc'
      --      in if lLPY > minY + defaultBoundingBoxHeight then [((lLPX + 0.1, lLPY - defaultBoundingBoxHeight * 0.5), (lLPX, minY))] else []
      --   _ -> []
      rc' = connectionsV2' r
      rc'' = case rc' of
        [] -> []
        _ ->
          case gCId of
            Nothing ->
              let (rLPX, rLPY) = snd $ last rc'
               in if rLPY > minY + defaultBoundingBoxHeight then [((rLPX, rLPY - defaultBoundingBoxHeight * 0.5), (rLPX, minY))] else []
            _ -> []
   in lc <> rc <> lc' <> rc' <> lc'' <> rc''
connectionsV2'' _ = []

connectionsV2' :: [PositionedBlock] -> [((Double, Double), (Double, Double))]
connectionsV2' [] = []
connectionsV2' [pB] = connectionsV2'' pB
connectionsV2' (pB1 : pB2 : pBs) =
  case pB1 of
    (PositionedFork _i _pId _c l _r _gCId x1 _x1R y1 maxX1 minYL1 minYR1 _lLY _lRY) ->
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

barebonesGamma'' :: PositionedBlock -> [((Int, Double, Double), ID, Double)]
barebonesGamma'' (PositionedFork _i pId _c l r gCId x xR y maxX minYL minYR _lLY lRY) =
  let lGamma = barebonesGamma' l
      rGamma = barebonesGamma' r
      gamma = case gCId of
        Nothing -> []
        (Just gCId') -> case r of
          [] -> [((pId, x, y), gCId', maxX)]
          _ -> [((pId, xR, minYR), gCId', maxX)]
   in rGamma <> lGamma <> gamma
barebonesGamma'' _ = []

-- TODO
-- While building barebones gamma connections, we need a positioned block identifier.
-- We don't have non-user-provided identifiers yet but we will need to add them.
-- Once barebones gamma connections are calculated and origins updated,
-- we also should update minYR (I think only that?) of the positioned blocks identified by the provided positioned block identifier.
barebonesGamma' :: [PositionedBlock] -> [((Int, Double, Double), ID, Double)]
barebonesGamma' = foldr (\pB accuGamma -> accuGamma <> barebonesGamma'' pB) []

barebonesGamma :: [[PositionedBlock]] -> [((Int, Double, Double), ID, Double)]
barebonesGamma = foldr (\pBs accuGamma -> accuGamma <> barebonesGamma' pBs) []

repositionOriginsTopY :: Double -> [((Int, Double, Double), ID, Double)] -> [((Int, Double, Double), ID, Double)]
repositionOriginsTopY _y [] = []
repositionOriginsTopY y (o@((pId, originX, originY), gCId, maxXOrigin) : os) =
  ( ( pId,
      originX,
      if originY <= y then originY - repositionShift else originY
    ),
    gCId,
    maxXOrigin
  )
    : repositionOriginsTopY y os

repositionOrigins :: Double -> Double -> [((Int, Double, Double), ID, Double)] -> [((Int, Double, Double), ID, Double)]
repositionOrigins _x _y [] = []
repositionOrigins x y (o@((pId, originX, originY), gCId, maxXOrigin) : os) =
  ( ( pId,
      if originX >= x then originX + repositionShift else originX,
      if originY < y then originY - repositionShift else originY
    ),
    gCId,
    maxXOrigin
  )
    : repositionOrigins x y os
