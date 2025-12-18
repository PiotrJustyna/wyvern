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

newRender'' ::
  Block ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  Double ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
newRender'' fork@(Fork i c l r gCId) o@(P (V2 oX oY)) ds gCs globalMaxWidth =
  let (gCW, gCH) = case gCId of
        Nothing -> (0.0, 0.0)
        Just _ -> (0.1, 0.1)
      (dQ, wQ, hQ) = (position [(o, wyvernHex c)], oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
      lX = oX
      lY = hQ
      rX = maxWL
      rY = hQ
      (dL, dsL, gCsL, wL, hL, maxWL, minHL) = newRender' l (p2 (lX, lY)) ds gCs 0.0
      (dR, dsR, gCsR, wR, hR, maxWR, minHR) = newRender' r (p2 (rX, if null r then oY else rY)) dsL gCsL globalMaxWidth
      newMaxW = maxWR + gCW
      newMaxW' = max newMaxW globalMaxWidth
      newMinH = min minHL minHR - gCH
      gCs' = case gCId of
        Nothing -> gCsR
        Just gCId' -> (p2 (wR, oY), newMaxW', if null r then hR else newMinH + defaultBoundingBoxHeight * 0.5, gCId') : gCsR
   in ( dQ
          <> dL
          <> dR
          <> (if null l then mempty else renderAlphaConnection [p2 (oX, hQ), o]) -- question -> left branch connection
          <> renderAlphaConnection (if null r then [p2 (rX, oY), o] else [p2 (rX, rY), p2 (rX, oY), o]) -- question -> right branch connection
          <> ( case gCId of
                 Nothing -> renderAlphaConnection [p2 (lX, newMinH + defaultBoundingBoxHeight * 0.5), p2 (rX, newMinH + defaultBoundingBoxHeight * 0.5), p2 (rX, hR)] -- right branch -> bottom of the fork connection
                 _ -> mempty
             ),
        dsR,
        gCs',
        wR,
        hR,
        newMaxW',
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
newRender' [] o@(P (V2 oX oY)) ds gCs globalWidth =
  let w = oX
      minD = oY
      localWidth = w + defaultBoundingBoxWidth
      localWidth' = max globalWidth localWidth
   in (mempty, ds, gCs, w, minD, localWidth', minD)
newRender' [b] o@(P (V2 oX oY)) ds gCs globalWidth =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', w, h, maxW, minH) = newRender'' b o ds' gCs globalWidth
   in (d, ds'', gCs', oX, h, maxW, minH)
newRender' (b : bs) o@(P (V2 oX oY)) ds gCs globalWidth =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', w, h, maxW, minH) = newRender'' b o ds' gCs globalWidth
      (d', ds''', gCs'', w', h', maxW', minH') = newRender' bs (p2 (oX, minH)) ds'' gCs' (maxW + 0.1)
      maxW'' = max maxW maxW'
   in (d <> d' <> renderAlphaConnection [p2 (oX, minH), p2 (oX, oY)], ds''', gCs'', w', h', maxW'', minH')

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
newRender accuRD accuDs accuBCs@(uBCs, lBCs, minD) accuGCs accuW accuH accuMaxH [b] =
  let (rD, ds, gCs, w, h, maxW, maxH) = newRender' b (p2 (accuW, accuH)) accuDs accuGCs accuW
   in newRender
        (accuRD <> rD)
        ds
        (uBCs, lBCs, min minD maxH)
        gCs
        maxW
        accuH
        (min minD maxH)
        []
newRender accuRD accuDs (uBCs, lBCs, minD) accuGCs accuW accuH accuMaxH (b : bs) =
  let (rD, ds, gCs, w, h, maxW, maxH) = newRender' b (p2 (accuW, accuH)) accuDs accuGCs accuW
   in newRender
        (accuRD <> rD)
        ds
        ((accuW, maxW) : uBCs, (accuW, maxW, h) : lBCs, min minD maxH)
        gCs
        maxW
        accuH
        (min minD maxH)
        bs

newRender1' :: [[Block]] -> (Diagram B, Map ID (Point V2 Double), ([(Double, Double)], [(Double, Double, Double)], Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, [[Block]])
newRender1' (b : bs) =
  let (rD, ds, (uBCs, lBCs, minD), gCs, w, h, maxH, _) = newRender mempty Data.Map.empty ([], [], 0.0) [] 0.0 0.0 0.0 [b]
      uBCs' = if null bs then uBCs else (0.0, w) : uBCs
      lBCs' = if null bs then lBCs else (0.0, w, maxH) : lBCs
      (rD', ds', (uBCs'', lBCs'', minD'), gCs', w', h', maxH', _) = newRender rD ds (uBCs', lBCs', minD) gCs w (negate defaultBoundingBoxHeight) maxH bs
   in (rD', ds', (uBCs'', lBCs'', minD'), gCs', w', h', maxH', b : bs)

newRender1 :: [[Block]] -> Diagram B
newRender1 [] = mempty
newRender1 bs =
  let (rD', ds', (uBCs, lBCs, minD), gCs', w', h', maxH', _) = newRender1' bs
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
