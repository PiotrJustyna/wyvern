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
import HelperDiagrams (renderText', renderedConnection, wyvernHeadline, wyvernRect, wyvernRoundedRect)
import ID
import qualified SkewerBlock (SkewerBlock (Action))

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
          Fork id content l r rId -> Fork id content (reverse' l) (reverse' r) rId : accu
          _ -> x : accu
    )
    []

updateOrigins :: (Maybe ID) -> (Point V2 Double) -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
updateOrigins i o origins =
  case i of
    Nothing -> origins
    Just i' -> Data.Map.insert i' o origins

updateDestinations :: (Maybe ID) -> (Point V2 Double) -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
updateDestinations i o origins =
  case i of
    Nothing -> origins
    Just i' -> Data.Map.insert i' o origins

updateGammaConnections :: Point V2 Double -> Maybe ID -> [(Point V2 Double, ID)] -> [(Point V2 Double, ID)]
updateGammaConnections gammaConnectionOrigin Nothing dGCs = dGCs
updateGammaConnections gammaConnectionOrigin (Just i) dGCs = (gammaConnectionOrigin, i) : dGCs

updateGammaConnections' ::
  Point V2 Double ->
  Double ->
  Double ->
  Block ->
  [(Point V2 Double, Double, Double, ID)] ->
  [(Point V2 Double, Double, Double, ID)]
updateGammaConnections' gCO maxX maxY (Fork _ _ _ _ (Just gCID)) gCs = (gCO, maxX, maxY, gCID) : gCs
updateGammaConnections' _ _ _ _ gCs = gCs

formulateGammaConnection :: Point V2 Double -> Point V2 Double -> Double -> Double -> [Point V2 Double]
formulateGammaConnection gO@(P (V2 gOX gOY)) gD@(P (V2 gDX gDY)) maxX maxY =
  let gammaMidpoint1 = p2 (gOX, maxY)
      gammaMidpoint2 = p2 (maxX - defaultBoundingBoxWidth * 0.5, maxY)
      gammaMidpoint3 = p2 (maxX - defaultBoundingBoxWidth * 0.5, gDY)
   in [gO, gammaMidpoint1, gammaMidpoint2, gammaMidpoint3, gD]

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
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
newRender'' fork@(Fork i c l r gCId) o@(P (V2 oX oY)) ds gCs =
  let (gCW, gCH) = case gCId of
        Nothing -> (0.0, 0.0)
        Just _ -> (0.1, 0.1)
      (dQ, wQ, hQ) = (position [(o, wyvernRect c)], oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
      (dL, dsL, gCsL, wL, hL, maxWL, minHL) = newRender' l (p2 (oX, hQ)) ds gCs
      (dR, dsR, gCsR, wR, hR, maxWR, minHR) = newRender' r (p2 (maxWL, hQ)) dsL gCsL
      newMaxW = maxWR + gCW
      newMinH = minHR - gCH
   in ( dQ <> dL <> dR,
        dsR,
        gCsR,
        wR,
        hR,
        newMaxW,
        if minHL < newMinH then minHL else newMinH
      )
newRender'' b o@(P (V2 oX oY)) ds gCs =
  ( position [(o, wyvernRect $ getContent b)],
    ds,
    gCs,
    oX,
    oY,
    oX + defaultBoundingBoxWidth,
    oY - defaultBoundingBoxHeight
  )

newRender' ::
  [Block] ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Double, Double, ID)] ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
newRender' [] o@(P (V2 oX oY)) ds gCs = (mempty, ds, gCs, oX, oY, oX, oY)
newRender' (b : []) o@(P (V2 oX oY)) ds gCs =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', w, h, maxW, maxH) = newRender'' b o ds' gCs
      gCs'' = updateGammaConnections' (p2 (w, h)) maxW maxH b gCs' -- oX
   in (d, ds'', gCs'', oX, maxH, maxW, maxH) -- oX
newRender' (b : bs) o@(P (V2 oX _oY)) ds gCs =
  let ds' = updateDestinations (getIdentifier b) o ds
      (d, ds'', gCs', w, h, maxW, maxH) = newRender'' b o ds' gCs
      gCs'' = updateGammaConnections' (p2 (w, h)) maxW maxH b gCs'
      (d', ds''', gCs''', w', h', maxW', maxH') = newRender' bs (p2 (oX, maxH)) ds'' gCs''
   in (d <> d', ds''', gCs''', w', h', if maxW > maxW' then maxW else maxW', maxH')

newRender :: [Block] -> Diagram B
newRender bs =
  let (rD, ds, gCs, w, h, maxW, maxH) = newRender' bs (p2 (0.0, 0.0)) Data.Map.empty []
   in foldl
        ( \accu (gCO, maxX, maxY, i) ->
            case Data.Map.lookup i ds of
              Nothing -> error $ "gamma connection destination " <> (show i) <> "not found in the collection of destinations: " <> (show ds)
              Just gCD -> accu <> (renderedConnection $ formulateGammaConnection gCO gCD maxX maxY)
        )
        rD
        gCs
