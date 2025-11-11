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

updateOrigins :: (Maybe ID) -> (Point V2 Double) -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
updateOrigins i o origins =
  case i of
    Nothing -> origins
    Just i' -> Data.Map.insert i' o origins

updateDimensions :: (Maybe ID) -> Double -> Map ID Double -> Map ID Double
updateDimensions (Just i) w ds =
  case Data.Map.lookup i ds of
    Nothing -> Data.Map.insert i (w) ds
    Just d -> Data.Map.insert i (d + w) ds
updateDimensions _ _ ds = ds

-- abc :: Block -> Block -> max width
-- abc :: ID -> ID -> max width

-- do it level by level
-- actual: 2 levels
-- expected: 20 levels
width'' :: Map ID Double -> [Block] -> Map Double Double
width'' gCs bs = case bs of
  [] -> Data.Map.empty
  bs' -> snd $ foldl (\(i, widths) w -> (i + 1, Data.Map.insert i w widths)) (0.0, Data.Map.empty) (map (width gCs) bs')

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
                  Nothing -> Data.Map.insert gCI (0.1) rDs
                  Just w -> Data.Map.insert gCI (w + 0.1) rDs
          _ -> accu
    )
    ds
    bs

dimensions :: [Block] -> Map ID Double
dimensions = dimensions' empty

updateGammaConnections :: Point V2 Double -> Maybe ID -> [(Point V2 Double, ID)] -> [(Point V2 Double, ID)]
updateGammaConnections gammaConnectionOrigin Nothing dGCs = dGCs
updateGammaConnections gammaConnectionOrigin (Just i) dGCs = (gammaConnectionOrigin, i) : dGCs

formulateGammaConnection :: Point V2 Double -> Point V2 Double -> Double -> [Point V2 Double]
formulateGammaConnection gO@(P (V2 gOX gOY)) gD@(P (V2 gDX gDY)) dWidth =
  let gammaMidpoint1 = p2 (gOX, gOY - defaultBoundingBoxHeight * 0.5)
      gammaMidpoint2 = p2 (gDX + dWidth - defaultBoundingBoxWidth * 0.5, gOY - defaultBoundingBoxHeight * 0.5)
      gammaMidpoint3 = p2 (gDX + dWidth - defaultBoundingBoxWidth * 0.5, gDY)
   in [gO, gammaMidpoint1, gammaMidpoint2, gammaMidpoint3, gD]

render' :: Block -> (Point V2 Double) -> Map ID (Point V2 Double) -> Map ID Double -> [(Point V2 Double, ID)] -> (Diagram B, Map ID (Point V2 Double), Map ID Double, [(Point V2 Double, ID)])
render' StartTerminator o@(P (V2 x _y)) _os _ds _dGCs = (position [(o, wyvernRoundedRect "start")], _os, _ds, _dGCs)
render' EndTerminator o@(P (V2 x _y)) _os _ds _dGCs = (position [(o, wyvernRoundedRect "end")], _os, _ds, _dGCs)
render' (Action i c) o@(P (V2 x _y)) os ds _dGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateDimensions i defaultBoundingBoxWidth ds, _dGCs)
render' (Headline i c) o@(P (V2 x y)) os ds _dGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateDimensions i defaultBoundingBoxWidth ds, _dGCs)
render' (Address i c) o@(P (V2 x y)) os ds _dGCs = (position [(o, wyvernRect c)], updateOrigins i o os, updateDimensions i defaultBoundingBoxWidth ds, _dGCs)
render' fork@(Fork i c l r gCId) o@(P (V2 x y)) origins ds dGCs =
  let lW = width' ds l
      rW = width' ds r
      lO = p2 (x, y - defaultBoundingBoxHeight)
      rO@(P (V2 r0x r0y)) = p2 (x + lW, y - defaultBoundingBoxHeight)
      origins' = updateOrigins i o origins
      ds' = updateDimensions i (lW + rW) ds
      dGCs' = updateGammaConnections (p2 (r0x, y - heightInUnits' r)) gCId dGCs
      (renderedL, originsL, dsL, dGCsL) = render l lO origins' ds' dGCs'
      (renderedR, originsLR, dsR, dGCsLR) = render r rO originsL dsL dGCsL
   in ( position [(p2 (x, y), wyvernRect c)]
          <> renderedL
          <> renderedR,
        originsLR,
        dsR,
        dGCsLR
      )

render :: [Block] -> (Point V2 Double) -> Map ID (Point V2 Double) -> Map ID Double -> [(Point V2 Double, ID)] -> (Diagram B, Map ID (Point V2 Double), Map ID Double, [(Point V2 Double, ID)])
render blocks o origins ds dGCs =
  let (diagram, _, origins', accuDs', dGCs') =
        foldl
          ( \(d, (P (V2 x y)), accuOrigins, accuDs, accuDGCs) block ->
              let h = height block
                  x' = x
                  y' = y - h
                  (d', accuOrigins', accuDs', accuDGCs') =
                    render' block (p2 (x, y)) accuOrigins accuDs accuDGCs
               in (d <> d', p2 (x', y'), accuOrigins', accuDs', accuDGCs')
          )
          (mempty, o, origins, ds, dGCs)
          blocks
   in (diagram, origins', accuDs', dGCs')

destinationLookup :: ID -> Map ID (Point V2 Double) -> Map ID Double -> (Point V2 Double, Double)
destinationLookup i os ds =
  case Data.Map.lookup i os of
    Nothing -> error $ (show i) <> " does not exist in the list of origins: " <> show os
    Just o -> case Data.Map.lookup i ds of
      Nothing -> error $ (show i) <> " does not exist in the list of dimensions: " <> show ds
      Just d -> (o, d)

firstAvailableSpace :: Double -> Double -> HashSet Double -> Double
firstAvailableSpace x w cs = case Data.HashSet.member (x + w) cs of
  False -> w
  _ -> firstAvailableSpace x (w - 0.1) cs

getY :: Point V2 Double -> Double
getY (P (V2 _ y)) = y

groupAndSort :: [(Point V2 Double, ID)] -> [(Point V2 Double, ID)]
groupAndSort xs =
  concatMap (sortBy (comparing (getY . fst)))
    . groupBy (\a b -> snd a == snd b)
    . sortBy (comparing snd)
    $ xs

renderAll :: Diagram B -> Map ID (Point V2 Double) -> Map ID Double -> [(Point V2 Double, ID)] -> HashSet Double -> (Diagram B, HashSet Double)
renderAll d os ds dGCs claimedSpace =
  let (dGCs', claimedSpace') =
        foldl
          ( \(accuDGCs, accuClaimedSpace) dGC ->
              let dGCO = fst dGC
                  (dGCD@(P (V2 x _)), w) = destinationLookup (snd dGC) os ds
                  w' = firstAvailableSpace x w accuClaimedSpace
               in ((formulateGammaConnection dGCO dGCD (w' + 3.1)) : accuDGCs, Data.HashSet.insert (x + w') accuClaimedSpace)
          )
          ([], claimedSpace)
          (groupAndSort dGCs)
      c = foldl (\accu x -> accu <> renderedConnection x) d dGCs'
   in (c, claimedSpace')
