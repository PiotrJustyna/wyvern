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
import HelperDiagrams (renderConnection, renderGammaConnection, renderText', wyvernHeadline, wyvernRect, wyvernRoundedRect)
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

formulateGammaConnection :: Point V2 Double -> Point V2 Double -> Double -> Double -> [Point V2 Double]
formulateGammaConnection gO@(P (V2 gOX gOY)) gD@(P (V2 gDX gDY)) maxX minY =
  let gD'@(P (V2 _gDX' gDY')) = p2 (gDX + (0.087 / 2.0) + 0.04, gDY + defaultBoundingBoxHeight * 0.5)
      gammaMidpoint1 = p2 (gOX, minY)
      gammaMidpoint2 = p2 (maxX - defaultBoundingBoxWidth * 0.5, minY)
      gammaMidpoint3 = p2 (maxX - defaultBoundingBoxWidth * 0.5, gDY')
   in [gO, gammaMidpoint1, gammaMidpoint2, gammaMidpoint3, gD']

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

newRender'' :: Block -> Point V2 Double -> Map ID (Point V2 Double) -> [(Point V2 Double, Double, Double, ID)] -> Double -> (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
newRender'' fork@(Fork i c l r gCId) o@(P (V2 oX oY)) ds gCs abc =
  let (gCW, gCH) = case gCId of
        Nothing -> (0.1, 0.1)
        Just _ -> (0.1, 0.1)
      (dQ, wQ, hQ) = (position [(o, wyvernRect c)], oX + defaultBoundingBoxWidth, oY - defaultBoundingBoxHeight)
      (dL, dsL, gCsL, wL, hL, maxWL, minHL) = newRender' l (p2 (oX, hQ)) ds gCs 0.0
      (dR, dsR, gCsR, wR, hR, maxWR, minHR) = newRender' r (p2 (maxWL, hQ)) dsL gCsL abc
      newMaxW = maxWR + gCW
      newMinH = (if minHL < minHR then minHL else minHR) - gCH
   in ( dQ <> dL <> dR,
        dsR,
        gCsR,
        wR,
        hR + defaultBoundingBoxHeight,
        newMaxW,
        newMinH
      )
newRender'' b o@(P (V2 oX oY)) ds gCs abc =
  ( position [(o, wyvernRect $ getContent b)],
    ds,
    gCs,
    oX,
    oY,
    oX + defaultBoundingBoxWidth,
    oY - defaultBoundingBoxHeight
  )

newRender' :: [Block] -> Point V2 Double -> Map ID (Point V2 Double) -> [(Point V2 Double, Double, Double, ID)] -> Double -> (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Double, Double, ID)], Double, Double, Double, Double)
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
      (d', ds''', gCs''', w', h', maxW'', minH') = newRender' bs (p2 (oX, minH)) ds'' gCs'' maxW' -- used to be: maxW' + 0.1
      maxW''' = if maxW' > maxW'' then maxW' else maxW''
   in (d <> d', ds''', gCs''', w', h', maxW''', minH')

newRender :: [Block] -> Diagram B
newRender bs =
  let (rD, ds, gCs, w, h, maxW, maxH) = newRender' bs (p2 (0.0, 0.0)) Data.Map.empty [] 0.0
   in foldl
        ( \accu (gCO, maxX, maxY, i) ->
            case Data.Map.lookup i ds of
              Nothing -> error $ "gamma connection destination " <> (show i) <> "not found in the collection of destinations: " <> (show ds)
              Just gCD -> accu <> (renderGammaConnection $ formulateGammaConnection gCO gCD maxX maxY)
        )
        rD
        gCs
