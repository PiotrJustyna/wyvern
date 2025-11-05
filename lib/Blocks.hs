module Blocks where

import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, heightRatio, widthRatio)
import Content (Content (..))
import Data.Map (Map, empty, insert, lookup)
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

updateOrigins ::
  (Maybe ID) ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  Map ID (Point V2 Double)
updateOrigins i o origins =
  case i of
    Nothing -> origins
    Just i' -> insert i' o origins

updateDimensions ::
  (Maybe ID) ->
  Double ->
  Map ID Double ->
  Map ID Double
updateDimensions (Just i) w ds =
  case Data.Map.lookup i ds of
    Nothing -> Data.Map.insert i (w) ds
    Just d -> Data.Map.insert i (d + w) ds
updateDimensions _ _ ds = ds

width' :: Map ID Double -> [Block] -> Double
width' gCs bs = case bs of
  [] -> defaultBoundingBoxWidth
  bs' -> maximum $ map (width gCs) bs'

width :: Map ID Double -> Block -> Double
width gCs (Fork i _ l r _) =
  let lW = width' gCs l
      rW = width' gCs r
      cW = case i of
        Nothing -> 0.0
        Just i' -> case Data.Map.lookup i' gCs of
          Nothing -> 0.0
          Just cW' -> cW'
   in lW + rW + cW
width _ _ = defaultBoundingBoxWidth

dimensions' :: Map ID Double -> [Block] -> Map ID Double
dimensions' ds bs =
  foldl
    ( \accu b ->
        case b of
          (Fork _ _ l r (Just gCI)) ->
            let lDs = dimensions' accu l
                rDs = dimensions' lDs r
             in case Data.Map.lookup gCI rDs of
                  Nothing -> insert gCI (0.1) rDs
                  Just w -> insert gCI (w + 0.1) rDs
          _ -> accu
    )
    ds
    bs

dimensions :: [Block] -> Map ID Double
dimensions = dimensions' empty

updateGammaConnections ::
  (Point V2 Double) ->
  (Maybe ID) ->
  Map ID (Point V2 Double) ->
  Map ID Double ->
  [(Point V2 Double, Point V2 Double)] ->
  [[Point V2 Double]] ->
  ([(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
updateGammaConnections gammaConnectionOrigin i origins dimensions dGCs iGCs =
  case i of
    Nothing -> (dGCs, iGCs)
    Just i' -> case Data.Map.lookup i' origins of
      Nothing -> error $ show i' <> " does not exist in the collection of origins: " <> show origins
      Just gammaConnectionDestination -> case Data.Map.lookup i' dimensions of
        Nothing -> error $ show i' <> " does not exist in the collection of dimensions: " <> show dimensions
        Just destinationWidth ->
          ( (gammaConnectionOrigin, gammaConnectionDestination) : dGCs,
            (formulateGammaConnection gammaConnectionOrigin gammaConnectionDestination destinationWidth) : iGCs
          )

formulateGammaConnection :: Point V2 Double -> Point V2 Double -> Double -> [Point V2 Double]
formulateGammaConnection gO@(P (V2 gOX gOY)) gD@(P (V2 gDX gDY)) dWidth =
  let gammaMidpoint1 = p2 (gOX, gOY - defaultBoundingBoxHeight * 0.5)
      gammaMidpoint2 = p2 (gDX + dWidth - defaultBoundingBoxWidth * 0.5, gOY - defaultBoundingBoxHeight * 0.5)
      gammaMidpoint3 = p2 (gDX + dWidth - defaultBoundingBoxWidth * 0.5, gDY)
   in [gO, gammaMidpoint1, gammaMidpoint2, gammaMidpoint3, gD]

-- instead of this:
-- [(Point V2 Double, Point V2 Double)], [[Point V2 Double]]
-- track something like this:
-- [(Point V2 Double, ID)]
-- translates to:
-- [(gamma connection origin coordinates, gamma connection destination ID)]
-- maybe we can combine two maps? origins and widths into destinations: Map ID (Point V2 Double)
render' ::
  Block ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  Map ID Double ->
  [(Point V2 Double, Point V2 Double)] ->
  [[Point V2 Double]] ->
  (Diagram B, Map ID (Point V2 Double), Map ID Double, [(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
render' StartTerminator o@(P (V2 x _y)) _os _ds _dGCs _iGCs = (position [(o, wyvernRoundedRect "start")], _os, _ds, _dGCs, _iGCs)
render' EndTerminator o@(P (V2 x _y)) _os _ds _dGCs _iGCs = (position [(o, wyvernRoundedRect "end")], _os, _ds, _dGCs, _iGCs)
render' (Action i c) o@(P (V2 x _y)) os ds _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateDimensions i defaultBoundingBoxWidth ds, _dGCs, _iGCs)
render' (Headline i c) o@(P (V2 x y)) os ds _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateDimensions i defaultBoundingBoxWidth ds, _dGCs, _iGCs)
render' (Address i c) o@(P (V2 x y)) os ds _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateDimensions i defaultBoundingBoxWidth ds, _dGCs, _iGCs)
render' fork@(Fork i c l r gCId) o@(P (V2 x y)) origins ds dGCs iGCs =
  let lW = width' ds l
      rW = width' ds r
      lO = p2 (x, y - defaultBoundingBoxHeight)
      rO@(P (V2 r0x r0y)) = p2 (x + lW, y - defaultBoundingBoxHeight)
      origins' = updateOrigins i o origins
      ds' = updateDimensions i (lW + rW) ds
      (dGCs', iGCs') = updateGammaConnections (p2 (r0x, y - heightInUnits' r)) gCId origins' ds' dGCs iGCs
      (renderedL, originsL, dsL, dGCsL, iGCsL) = render l lO origins' ds' dGCs' iGCs'
      (renderedR, originsLR, dsR, dGCsLR, iGCsLR) = render r rO originsL dsL dGCsL iGCsL
   in ( position [(p2 (x, y), wyvernRect c)]
          <> renderedL
          <> renderedR,
        originsLR,
        dsR,
        dGCsLR,
        iGCsLR
      )

render ::
  [Block] ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  Map ID Double ->
  [(Point V2 Double, Point V2 Double)] ->
  [[Point V2 Double]] ->
  (Diagram B, Map ID (Point V2 Double), Map ID Double, [(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
render blocks o origins ds dGCs iGCs =
  let (diagram, _, origins', accuDs', dGCs', iGCs') =
        foldl
          ( \(d, (P (V2 x y)), accuOrigins, accuDs, accuDGCs, accuIGCs) block ->
              let h = height block
                  x' = x
                  y' = y - h
                  (d', accuOrigins', accuDs', accuDGCs', accuIGCs') =
                    render' block (p2 (x, y)) accuOrigins accuDs accuDGCs accuIGCs
               in (d <> d', p2 (x', y'), accuOrigins', accuDs', accuDGCs', accuIGCs')
          )
          (mempty, o, origins, ds, dGCs, iGCs)
          blocks
   in (diagram, origins', accuDs', dGCs', iGCs')

renderAll :: Diagram B -> [[Point V2 Double]] -> Diagram B
renderAll d iGCs = d <> foldl (\accu x -> accu <> renderedConnection x) mempty iGCs
