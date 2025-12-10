module Blocks where

import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, heightRatio, widthRatio)
import Content (Content (..))
import Data.HashSet (HashSet, insert, member)
import Data.List (groupBy, sortBy, sortOn)
import Data.Map (Map, empty, insert, lookup)
import Data.Ord (comparing)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position, r2, translate, (#))
import GHC.Float
import HelperDiagrams (renderAlphaConnection, renderGammaConnection, renderLowerBetaConnections, renderSideBetaConnection, renderText', renderUpperBetaConnections, wyvernHeadline, wyvernHex, wyvernRect, wyvernRoundedRect)
import ID

data Block
  = StartTerminator
  | Action (Maybe ID) String
  | Headline (Maybe ID) String
  | Address (Maybe ID) String
  | Fork (Maybe ID) String [Block] [Block] (Maybe ID)
  | EndTerminator
  deriving (Show)

reverse'' :: [Block] -> [Block]
reverse'' bs =
  let res =
        foldl
          ( \accu x ->
              case x of
                Fork id content l r rId -> Fork id content (reverse'' l) (reverse'' r) rId : accu
                _ -> x : accu
          )
          []
          bs
   in res

reverse' :: [[Block]] -> [[Block]]
reverse' = foldl (\accu x -> reverse'' x : accu) []

updateDestinations :: (Maybe ID) -> (Point V2 Double) -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
updateDestinations i o origins =
  case i of
    Nothing -> origins
    Just i' -> Data.Map.insert i' o origins

updateGammaConnections' ::
  Point V2 Double ->
  Double ->
  Double ->
  Block ->
  [(Point V2 Double, Double, Double, ID)] ->
  [(Point V2 Double, Double, Double, ID)]
updateGammaConnections' gCO maxX maxY (Fork _ _ _ _ (Just gCID)) gCs = (gCO, maxX, maxY, gCID) : gCs
updateGammaConnections' _ _ _ _ gCs = gCs

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

newRender'' ::
  Block ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  Double ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
newRender'' fork@(Fork i c l r gCId) o@(P (V2 oX oY)) ds gCs abc =
  let (gCW, gCH) = case gCId of
        Nothing -> (0.1, 0.1)
        Just _ -> (0.1, 0.1)
      (dQ, wQ, hQ) = (position [(o, wyvernHex c)], oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
      (dL, dsL, gCsL, wL, hL, maxWL, minHL) = newRender' l (p2 (oX, hQ)) ds gCs 0.0
      (dR, dsR, gCsR, wR, hR, maxWR, minHR) = newRender' r (p2 (maxWL, hQ)) dsL gCsL abc
      newMaxW = maxWR + gCW
      newMinH = (if minHL < minHR then minHL else minHR) - gCH
   in ( dQ
          <> dL
          <> dR
          <> (renderAlphaConnection [p2 (oX, hQ), o])
          <> (renderAlphaConnection [p2 (oX, hQ), p2 (oX, hR + defaultBoundingBoxHeight - gCH)])
          <> (renderAlphaConnection [p2 (maxWL, hQ), p2 (maxWL, oY), o])
          <> ( case gCId of
                 Nothing -> (renderAlphaConnection [p2 (oX, hR - gCH + defaultBoundingBoxHeight * 0.5), p2 (maxWL, hR - gCH + defaultBoundingBoxHeight * 0.5), p2 (maxWL, hQ)])
                 _ -> mempty
             ),
        dsR,
        gCsR,
        wR,
        hR + defaultBoundingBoxHeight,
        newMaxW,
        newMinH
      )
newRender'' StartTerminator o@(P (V2 oX oY)) ds gCs abc = (position [(o, wyvernRoundedRect $ getContent StartTerminator)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
newRender'' EndTerminator o@(P (V2 oX oY)) ds gCs abc = (position [(o, wyvernRoundedRect $ getContent EndTerminator)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
newRender'' b o@(P (V2 oX oY)) ds gCs abc = (position [(o, wyvernRect $ getContent b)], ds, gCs, oX, oY, oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)

newRender' ::
  [Block] ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  Double ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
newRender' [] o@(P (V2 oX oY)) ds gCs abc =
  let w = oX
      h = oY
      maxW = w + defaultBoundingBoxWidth
      minH = h
      maxW' = if abc > maxW then abc else maxW
   in (mempty, ds, gCs, w, h, maxW', minH)
newRender' (b : []) o@(P (V2 oX oY)) ds gCs abc =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', w, h, maxW, minH) = newRender'' b o ds' gCs abc
      maxW' = if abc > maxW then abc else maxW
      gCs'' = updateGammaConnections' (p2 (w, h)) maxW' (minH + defaultBoundingBoxHeight * 0.5) b gCs'
   in (d, ds'', gCs'', oX, minH, maxW', minH)
newRender' (b : bs) o@(P (V2 oX _oY)) ds gCs abc =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', w, h, maxW, minH) = newRender'' b o ds' gCs abc
      maxW' = if abc > maxW then abc else maxW
      gCs'' = updateGammaConnections' (p2 (w, h)) maxW' (minH + defaultBoundingBoxHeight * 0.5) b gCs'
      (d', ds''', gCs''', w', h', maxW'', minH') = newRender' bs (p2 (oX, minH)) ds'' gCs'' maxW'
      maxW''' = if maxW' > maxW'' then maxW' else maxW''
   in (d <> d' <> (renderAlphaConnection [p2 (oX, minH), o]), ds''', gCs''', w', h', maxW''', minH')

newRender ::
  Diagram B ->
  Map ID (Point V2 Double) ->
  ([(Double, Double)], [(Double, Double, Double)], Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  Double ->
  Double ->
  Double ->
  [[Block]] ->
  (Diagram B, Map ID (Point V2 Double), ([(Double, Double)], [(Double, Double, Double)], Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, [[Block]])
newRender rD ds bCs gCs w h maxH [] = (rD, ds, bCs, gCs, w, h, maxH, [])
newRender accuRD accuDs accuBCs@(uBCs, lBCs, minD) accuGCs accuW accuH accuMaxH (b : []) =
  let (rD, ds, gCs, w, h, maxW, maxH) = newRender' b (p2 (accuW, accuH)) accuDs accuGCs accuW
   in newRender
        (accuRD <> rD)
        ds
        (uBCs, lBCs, if minD < maxH then minD else maxH)
        gCs
        maxW
        accuH
        (if minD < maxH then minD else maxH)
        []
newRender accuRD accuDs (uBCs, lBCs, minD) accuGCs accuW accuH accuMaxH (b : bs) =
  let (rD, ds, gCs, w, h, maxW, maxH) = newRender' b (p2 (accuW, accuH)) accuDs accuGCs accuW
   in newRender
        (accuRD <> rD)
        ds
        (((accuW, maxW) : uBCs), ((accuW, maxW, h) : lBCs), if minD < maxH then minD else maxH)
        gCs
        maxW
        accuH
        (if minD < maxH then minD else maxH)
        bs

newRender1' :: [[Block]] -> (Diagram B, Map ID (Point V2 Double), ([(Double, Double)], [(Double, Double, Double)], Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, [[Block]])
newRender1' (b : bs) =
  let (rD, ds, (uBCs, lBCs, minD), gCs, w, h, maxH, _) = newRender mempty Data.Map.empty ([], [], 0.0) [] 0.0 0.0 0.0 [b]
      uBCs' = if null bs then uBCs else (0.0, w) : uBCs
      lBCs' = if null bs then lBCs else (0.0, w, maxH) : lBCs
      (rD', ds', (uBCs'', lBCs'', minD'), gCs', w', h', maxH', _) = newRender rD ds (uBCs', lBCs', minD) gCs w (0 - defaultBoundingBoxHeight) maxH bs
   in (rD', ds', (uBCs'', lBCs'', minD'), gCs', w', h', maxH', (b : bs))

newRender1 :: [[Block]] -> Diagram B
newRender1 [] = mempty
newRender1 bs =
  let (rD', ds', (uBCs, lBCs, minD), gCs', w', h', maxH', _) = newRender1' bs
      bCs = case length bs of
        1 -> mempty
        _ ->
          let rUBCs = renderUpperBetaConnections uBCs (0 - defaultBoundingBoxHeight)
              rSBC = renderSideBetaConnection (p2 (0.0, maxH' + defaultBoundingBoxHeight * 0.5)) (p2 (0.0, 0.0 + defaultBoundingBoxHeight * 0.5 - defaultBoundingBoxHeight))
              rLBCs = renderLowerBetaConnections lBCs (maxH' + defaultBoundingBoxHeight * 0.5)
           in rUBCs <> rSBC <> rLBCs
   in foldl
        ( \accu (gCO, maxX, maxY, i) ->
            case Data.Map.lookup i ds' of
              Nothing -> error $ "gamma connection destination " <> (show i) <> "not found in the collection of destinations: " <> (show ds')
              Just gCD -> accu <> (renderGammaConnection gCO gCD maxX maxY)
        )
        (rD' <> bCs)
        gCs'
