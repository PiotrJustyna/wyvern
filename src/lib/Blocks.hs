module Blocks where

import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Data.Map (Map, empty, insert, lookup)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position)
import HelperDiagrams (renderConnection, renderGammaConnection, renderLowerBetaConnections, renderSideBetaConnection, renderUpperBetaConnections, wyvernAddress, wyvernHeadline, wyvernQuestion, wyvernRect, wyvernRoundedRect)
import ID

data Block
  = StartTerminator
  | Action (Maybe ID) String
  | Headline (Maybe ID) String
  | Address (Maybe ID) String
  | Fork (Maybe ID) String [Block] [Block] (Maybe ID)
  | EndTerminator
  deriving (Show)

reverse' :: [Block] -> [Block]
reverse' =
  foldl
    ( \accu x ->
        case x of
          Fork i content l r rId -> Fork i content (reverse' l) (reverse' r) rId : accu
          _ -> x : accu
    )
    []

reverse :: [[Block]] -> [[Block]]
reverse = foldl (\accu x -> reverse' x : accu) []

updateDestinations :: Maybe ID -> Point V2 Double -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
updateDestinations i o origins =
  case i of
    Nothing -> origins
    Just i' -> Data.Map.insert i' o origins

getContent :: Block -> String
getContent StartTerminator = "start"
getContent EndTerminator = "end"
getContent (Action _ c) = c
getContent (Headline _ c) = c
getContent (Address _ c) = c
getContent (Fork _ c _ _ _) = c

getIdentifier :: Block -> Maybe ID
getIdentifier StartTerminator = Nothing
getIdentifier EndTerminator = Nothing
getIdentifier (Action i _) = i
getIdentifier (Headline i _) = i
getIdentifier (Address i _) = i
getIdentifier (Fork i _ _ _ _) = i

getAllIdentifiers' :: [Block] -> [ID]
getAllIdentifiers' = foldr (\singleBlock accu -> getAllIdentifiers singleBlock <> accu) []

getAllIdentifiers :: Block -> [ID]
getAllIdentifiers (Fork (Just i) _ l r _) = getAllIdentifiers' l <> getAllIdentifiers' r <> [i]
getAllIdentifiers (Fork Nothing _ l r _) = getAllIdentifiers' l <> getAllIdentifiers' r
getAllIdentifiers b =
  case getIdentifier b of
    Just i -> [i]
    Nothing -> []

renderSingleBlock ::
  Block ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  Double ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
renderSingleBlock (Fork _i c l r gCId) o@(P (V2 oX oY)) ds gCs globalMaxWidth =
  let (gCW, gCH) = case gCId of
        Nothing -> (0.0, 0.0)
        Just _ -> (0.1, 0.1)
      (dQ, _wQ, hQ) = (position [(o, wyvernQuestion c)], oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
      lX = oX
      lY = hQ
      rX = maxWL
      rY = hQ
      (dL, dsL, gCsL, _wL, _hL, maxWL, minHL) = renderSingleSkewer l (p2 (lX, lY)) ds gCs 0.0
      (dR, dsR, gCsR, wR, hR, maxWR, minHR) = renderSingleSkewer r (p2 (rX, if null r then oY else rY)) dsL gCsL globalMaxWidth
      newMaxW = maxWR + gCW
      newMaxW' = max newMaxW globalMaxWidth
      newMinH = min minHL minHR - gCH
      gCs' = case gCId of
        Nothing -> gCsR
        Just gCId' -> (p2 (wR, oY), newMaxW', if null r then hR else newMinH + defaultBoundingBoxHeight * 0.5, gCId') : gCsR
   in ( dQ
          <> dL
          <> dR
          <> (if null l then mempty else renderConnection [p2 (oX, hQ), o]) -- question -> left branch connection
          <> renderConnection (if null r then mempty else [p2 (oX, newMinH + defaultBoundingBoxHeight), p2 (oX, hQ)]) -- left branch -> bottom of the fork connection
          <> renderConnection (if null r then [p2 (rX, oY), o] else [p2 (rX, rY), p2 (rX, oY), o]) -- question -> right branch connection
          <> ( case gCId of
                 Nothing -> renderConnection [p2 (lX, newMinH + defaultBoundingBoxHeight * 0.5), p2 (rX, newMinH + defaultBoundingBoxHeight * 0.5), p2 (rX, hR)] -- right branch -> bottom of the fork connection
                 _ -> mempty
             ),
        dsR,
        gCs',
        wR,
        hR,
        newMaxW',
        newMinH
      )
renderSingleBlock StartTerminator o@(P (V2 oX oY)) ds gCs _globalMaxWidth = (position [(o, wyvernRoundedRect $ getContent StartTerminator)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
renderSingleBlock EndTerminator o@(P (V2 oX oY)) ds gCs _globalMaxWidth = (position [(o, wyvernRoundedRect $ getContent EndTerminator)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
renderSingleBlock (Headline _i c) o@(P (V2 oX oY)) ds gCs _globalMaxWidth = (position [(o, wyvernHeadline c)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
renderSingleBlock (Address _i c) o@(P (V2 oX oY)) ds gCs _globalMaxWidth = (position [(o, wyvernAddress c)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
renderSingleBlock b o@(P (V2 oX oY)) ds gCs _globalMaxWidth = (position [(o, wyvernRect $ getContent b)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)

renderSingleSkewer ::
  [Block] ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  Double ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
renderSingleSkewer [] (P (V2 oX oY)) ds gCs globalWidth =
  let w = oX
      minD = oY
      localWidth = w + defaultBoundingBoxWidth
      localWidth' = max globalWidth localWidth
   in (mempty, ds, gCs, w, minD, localWidth', minD)
renderSingleSkewer [b] o@(P (V2 oX _oY)) ds gCs globalWidth =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', _w, h, maxW, minH) = renderSingleBlock b o ds' gCs globalWidth
   in (d, ds'', gCs', oX, h, maxW, minH)
renderSingleSkewer (b : bs) o@(P (V2 oX oY)) ds gCs globalWidth =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', _w, _h, maxW, minH) = renderSingleBlock b o ds' gCs globalWidth
      (d', ds''', gCs'', w', h', maxW', minH') = renderSingleSkewer bs (p2 (oX, minH)) ds'' gCs' (maxW + 0.1)
   in (d <> d' <> renderConnection [p2 (oX, minH), p2 (oX, oY)], ds''', gCs'', w', h', max maxW maxW', minH')

renderAllSkewers' ::
  Diagram B ->
  Map ID (Point V2 Double) ->
  ([(Double, Double)], [(Double, Double, Double)], Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  Double ->
  Double ->
  Double ->
  [[Block]] ->
  (Diagram B, Map ID (Point V2 Double), ([(Double, Double)], [(Double, Double, Double)], Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, [[Block]])
renderAllSkewers' rD ds bCs gCs w h maxH [] = (rD, ds, bCs, gCs, w, h, maxH, [])
renderAllSkewers' accuRD accuDs (uBCs, lBCs, minD) accuGCs accuW accuH _accuMaxH [b] =
  let (rD, ds, gCs, _w, _h, maxW, maxH) = renderSingleSkewer b (p2 (accuW, accuH)) accuDs accuGCs accuW
   in renderAllSkewers' (accuRD <> rD) ds (uBCs, lBCs, min minD maxH) gCs maxW accuH (min minD maxH) []
renderAllSkewers' accuRD accuDs (uBCs, lBCs, minD) accuGCs accuW accuH _accuMaxH (b : bs) =
  let (rD, ds, gCs, _w, h, maxW, maxH) = renderSingleSkewer b (p2 (accuW, accuH)) accuDs accuGCs accuW
   in renderAllSkewers' (accuRD <> rD) ds ((accuW, maxW) : uBCs, (accuW, maxW, h) : lBCs, min minD maxH) gCs maxW accuH (min minD maxH) bs

renderAllSkewers :: [[Block]] -> (Diagram B, Map ID (Point V2 Double), ([(Double, Double)], [(Double, Double, Double)], Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, [[Block]])
renderAllSkewers [] = error "no skewers provided"
renderAllSkewers blocks@(b : bs) =
  let manyBlocks = length blocks > 2
      (rD, ds, (uBCs, lBCs, minD), gCs, w, _h, maxH, _) = renderAllSkewers' mempty Data.Map.empty ([], [], 0.0) [] 0.0 0.0 0.0 [b]
      uBCs' = if null bs then uBCs else (0.0, w) : uBCs
      lBCs' = if null bs then lBCs else (0.0, w, maxH) : lBCs
      (rD', ds', (uBCs'', lBCs'', minD'), gCs', w', h', maxH', _) = renderAllSkewers' rD ds (uBCs', lBCs', minD) gCs w (negate defaultBoundingBoxHeight) maxH bs
   in (rD', ds', (uBCs'', lBCs'', minD'), gCs', w', h', if manyBlocks then maxH' else maxH, b : bs)

renderDiagram' :: [[Block]] -> Diagram B
renderDiagram' [] = mempty
renderDiagram' bs =
  let (rD', ds', (uBCs, lBCs, _minD), gCs', _w', _h', maxH', _) = renderAllSkewers bs
      bCs = case length bs of
        1 -> mempty
        _ ->
          let rUBCs = renderUpperBetaConnections uBCs (negate defaultBoundingBoxHeight)
              rSBC = renderSideBetaConnection (p2 (0.0, maxH' + defaultBoundingBoxHeight * 0.5)) (p2 (0.0, 0.0 + defaultBoundingBoxHeight * 0.5 - defaultBoundingBoxHeight))
              rLBCs = renderLowerBetaConnections lBCs (maxH' + defaultBoundingBoxHeight * 0.5)
           in rUBCs <> rSBC <> rLBCs
   in foldl
        ( \accu (gCO, maxX, maxY, i) ->
            case Data.Map.lookup i ds' of
              Nothing -> error $ "gamma connection destination " <> show i <> "not found in the collection of destinations: " <> show ds'
              Just gCD -> accu <> renderGammaConnection gCO gCD maxX maxY
        )
        (rD' <> bCs)
        gCs'

renderDiagram :: [[Block]] -> Diagram B
renderDiagram [] = mempty
renderDiagram [b] = renderDiagram' $ Blocks.reverse [(EndTerminator : b) <> [StartTerminator]]
renderDiagram (b : bs) = renderDiagram' . Blocks.reverse $ ((EndTerminator : b) : init bs) <> [last bs <> [StartTerminator]]

-- 2026-04-15 PJ:
-- ==============
-- Validation also needs to be recursive.
validateSingleBlock :: Block -> [ID] -> (Bool, String)
validateSingleBlock (Fork (Just i) _ _ _ (Just gCId)) ids = if gCId == i || gCId `elem` ids then (True, "") else (False, "Gamma connection ID: " <> show gCId <> " does not exist in the collection of block identifiers.")
validateSingleBlock (Fork Nothing _ _ _ (Just gCId)) ids = if gCId `elem` ids then (True, "") else (False, "Gamma connection ID: " <> show gCId <> " does not exist in the collection of block identifiers.")
validateSingleBlock (Fork _ _ _ _ Nothing) _ = (True, "")
validateSingleBlock _ _ = (True, "")

idsAlreadyPresent :: [ID] -> [ID] -> [ID]
idsAlreadyPresent xs xs' = foldl (\accu x -> if x `elem` xs' then x : accu else accu) [] xs

validateBlocks' :: [Block] -> [ID] -> (Either [Block] [String], [ID])
validateBlocks' [] ids = (Left [], ids)
validateBlocks' [b] ids = (Left [b], getAllIdentifiers b <> ids) -- todo: also check for duplicates
validateBlocks' bs ids =
  let (errors, ids') =
        foldr
          ( \singleBlock (accuErrors, accuIds) ->
              let (isValid, errorMessage) = validateSingleBlock singleBlock accuIds
                  newIdentifiers = getAllIdentifiers singleBlock
                  newIdentifiersDuplicated = idsAlreadyPresent newIdentifiers accuIds
                  duplicatedIdentifiersErrors = case newIdentifiersDuplicated of
                    [] -> accuErrors
                    _ -> ("Following identifiers are duplicated: " <> (show newIdentifiersDuplicated)) : accuErrors
               in (if isValid then duplicatedIdentifiersErrors else errorMessage : duplicatedIdentifiersErrors, newIdentifiers <> accuIds)
          )
          ([], ids)
          bs
   in case errors of
        [] -> (Left bs, ids')
        _ -> (Right errors, ids')

validateBlocks :: [[Block]] -> (Either [[Block]] [String], [ID])
validateBlocks [] = (Left [], [])
validateBlocks [b] =
  case validateBlocks' b [] of
    (Left b', ids) -> (Left [b'], ids)
    (Right errors, ids) -> (Right errors, ids)
validateBlocks bs =
  let (allErrors, allIds) =
        foldl
          ( \(accuErrors, accuIds) b ->
              case validateBlocks' b accuIds of
                (Left _, ids) -> (accuErrors, ids <> accuIds)
                (Right errors, ids) -> (errors <> accuErrors, ids <> accuIds)
          )
          ([], [])
          bs
   in case allErrors of
        [] -> (Left bs, allIds)
        _ -> (Right allErrors, allIds)
