module Blocks where

import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, heightRatio, widthRatio)
import Content (Content (..))
import Data.Map (Map, insert, lookup)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position, r2, translate, (#))
import GHC.Float
import HelperDiagrams (renderText', wyvernHeadline, wyvernRect, wyvernRoundedRect)
import ID
import qualified SkewerBlock (SkewerBlock (Action))

data Block
  = StartTerminator
  | Action (Maybe ID) String
  | Headline (Maybe ID) String
  | Address (Maybe ID) String
  | Fork (Maybe ID) String [Block] [Block] (Maybe ID)
  | EndTerminator

reverse' :: [Block] -> [Block]
reverse' =
  foldl
    ( \accu x ->
        case x of
          Fork id content l r rId -> Fork id content (reverse' l) (reverse' r) rId : accu
          _ -> x : accu
    )
    []

heightInUnits' :: [Block] -> Double
heightInUnits' blocks = sum $ map height blocks

height :: Block -> Double
height (Fork _ _ l r _) =
  defaultBoundingBoxHeight
    + max
      ( if null l
          then 0.0
          else heightInUnits' l
      )
      ( if null r
          then 0.0
          else heightInUnits' r
      )
height _ = defaultBoundingBoxHeight

width' :: Block -> Double
width' (Fork _ _ l r Nothing) = width l + width r
-- 2025-10-06 PJ:
-- --------------
-- We can simplify as there is only one
-- type of connection allowed for now.
width' (Fork _ _ l r _) = width l + width r + 1.0
width' _ = defaultBoundingBoxWidth

width :: [Block] -> Double
width x =
  case x of
    [] -> defaultBoundingBoxWidth
    blocks -> maximum $ map width' blocks

updateOrigins ::
  (Maybe ID) ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  Map ID (Point V2 Double)
updateOrigins i o origins =
  case i of
    Nothing -> origins
    Just i' -> insert i' o origins

updateMaxX :: String -> Double -> [(String, Double)] -> [(String, Double)]
updateMaxX x y maxXs = (x, (y + defaultBoundingBoxWidth * 0.5)) : maxXs

scout' :: Block -> [(String, Double)]
scout' StartTerminator = [("start", defaultBoundingBoxWidth)]
scout' EndTerminator = [("end", defaultBoundingBoxWidth)]
scout' (Action _ c) = [(c, defaultBoundingBoxWidth)]
scout' (Headline _ c) = [(c, defaultBoundingBoxWidth)]
scout' (Address _ c) = [(c, defaultBoundingBoxWidth)]
scout' (Fork _ c l r _) =
  let scoutL = scout l
      scoutR = scout r
  in (c, (maximum (map snd scoutL)) + (maximum (map snd scoutR))) : scoutL <> scoutR

scout :: [Block] -> [(String, Double)]
scout blocks = foldl (\accuW block -> accuW <> scout' block) [] blocks

updateGammaConnections ::
  (Point V2 Double) ->
  (Maybe ID) ->
  Map ID (Point V2 Double) ->
  [(String, Double)] ->
  [(Point V2 Double, Point V2 Double)] ->
  [[Point V2 Double]] ->
  ([(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
updateGammaConnections gammaConnectionOrigin i origins maxXs dGCs iGCs =
  case i of
    Nothing -> (dGCs, iGCs)
    Just i' -> case Data.Map.lookup i' origins of
      Nothing -> error $ show i' <> " does not exist in the collection of origins: " <> show origins
      Just gammaConnectionDestination ->
        ( (gammaConnectionOrigin, gammaConnectionDestination) : dGCs,
          (formulateGammaConnection gammaConnectionOrigin gammaConnectionDestination maxXs) : iGCs
        )

formulateGammaConnection :: Point V2 Double -> Point V2 Double -> [(String, Double)] -> [Point V2 Double]
formulateGammaConnection gO@(P (V2 gOX gOY)) gD@(P (V2 gDX gDY)) maxXs =
  let maxXs' = maximum $ map snd maxXs
      gammaMidpoint1 = p2 (gOX, gOY - 0.1)
      gammaMidpoint2 = p2 (0.1 + maxXs', gOY - 0.1)
      gammaMidpoint3 = p2 (0.1 + maxXs', gDY)
   in [gO, gammaMidpoint1, gammaMidpoint2, gammaMidpoint3, gD]

render' ::
  Block ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  [(String, Double)] ->
  [(Point V2 Double, Point V2 Double)] ->
  [[Point V2 Double]] ->
  (Diagram B, Map ID (Point V2 Double), [(String, Double)], [(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
render' StartTerminator o@(P (V2 x _y)) _os maxXs _dGCs _iGCs = (position [(o, wyvernRoundedRect "start")], _os, updateMaxX "start" x maxXs, _dGCs, _iGCs)
render' EndTerminator o@(P (V2 x _y)) _os maxXs _dGCs _iGCs = (position [(o, wyvernRoundedRect "end")], _os, updateMaxX "end" x maxXs, _dGCs, _iGCs)
render' (Action i c) o@(P (V2 x _y)) os maxXs _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateMaxX c x maxXs, _dGCs, _iGCs)
render' (Headline i c) o@(P (V2 x y)) os maxXs _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateMaxX c x maxXs, _dGCs, _iGCs)
render' (Address i c) o@(P (V2 x y)) os maxXs _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateMaxX c x maxXs, _dGCs, _iGCs)
render' fork@(Fork i c l r gCId) o@(P (V2 x y)) origins maxXs dGCs iGCs =
  let lO = p2 (x, y - defaultBoundingBoxHeight)
      rO@(P (V2 r0x r0y)) = p2 (x + width l, y - defaultBoundingBoxHeight)
      origins' = updateOrigins i o origins
      (dGCs', iGCs') = updateGammaConnections (p2 (r0x, y - heightInUnits' r)) gCId origins' maxXs dGCs iGCs
      (renderedL, originsL, maxXsL, dGCsL, iGCsL) = render l lO origins' maxXs dGCs' iGCs'
      (renderedR, originsLR, maxXsR, dGCsLR, iGCsLR) = render r rO originsL maxXsL dGCsL iGCsL
   in ( position [(p2 (x, y), wyvernRect c)]
          <> renderedL
          <> renderedR,
        originsLR,
        maxXsR,
        dGCsLR,
        iGCsLR
      )

render ::
  [Block] ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  [(String, Double)] ->
  [(Point V2 Double, Point V2 Double)] ->
  [[Point V2 Double]] ->
  (Diagram B, Map ID (Point V2 Double), [(String, Double)], [(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
render blocks o origins maxXs dGCs iGCs =
  let (diagram, _, origins', maxXs', dGCs', iGCs') =
        foldl
          ( \(d, (P (V2 x y)), accuOrigins, accuMaxXs, accuDGCs, accuIGCs) block ->
              let h = height block
                  x' = x
                  y' = y - h
                  (d', accuOrigins', accuMaxXs', accuDGCs', accuIGCs') =
                    render' block (p2 (x, y)) accuOrigins accuMaxXs accuDGCs accuIGCs
               in (d <> d', p2 (x', y'), accuOrigins', accuMaxXs', accuDGCs', accuIGCs')
          )
          (mempty, o, origins, maxXs, dGCs, iGCs)
          blocks
   in (diagram, origins', maxXs', dGCs', iGCs')
