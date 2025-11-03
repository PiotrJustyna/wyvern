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

width' :: Map ID Double -> Block -> Double
width' widths (Fork i _ _ _ _) = blockWidth i widths
width' widths (Action i _) = blockWidth i widths
width' widths (Headline i _) = blockWidth i widths
width' widths _ = defaultBoundingBoxWidth

width :: [Block] -> Map ID Double -> Double
width x widths =
  case x of
    [] -> defaultBoundingBoxWidth
    blocks -> maximum $ map (width' widths) blocks

updateOrigins ::
  (Maybe ID) ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  Map ID (Point V2 Double)
updateOrigins i o origins =
  case i of
    Nothing -> origins
    Just i' -> insert i' o origins

blocksOnlyWidth'' :: Block -> [(Maybe ID, Double)]
blocksOnlyWidth'' (Action i _) = [(i, defaultBoundingBoxWidth)]
blocksOnlyWidth'' (Headline i _) = [(i, defaultBoundingBoxWidth)]
blocksOnlyWidth'' (Address i _) = [(i, defaultBoundingBoxWidth)]
blocksOnlyWidth'' (Fork i _ l r _) =
  let lW = blocksOnlyWidth' l
      maxL = case lW of
        [] -> 0.0
        lW' -> maximum (map snd lW)
      rW = blocksOnlyWidth' r
      maxR = case rW of
        [] -> 0.0
        rW' -> maximum (map snd rW)
   in (i, maxL + maxR) : lW <> rW
blocksOnlyWidth'' _ = [(Nothing, defaultBoundingBoxWidth)]

blocksOnlyWidth' :: [Block] -> [(Maybe ID, Double)]
blocksOnlyWidth' blocks = foldl (\accuW block -> accuW <> blocksOnlyWidth'' block) [] blocks

blocksOnlyWidth :: [Block] -> Map ID Double
blocksOnlyWidth blocks =
  foldl
    ( \accu x ->
        case x of
          (Nothing, _) -> accu
          (Just i, w) -> insert i w accu
    )
    empty
    (blocksOnlyWidth' blocks)

blocksOnlyWidthA :: [Block] -> Map ID [(Double, Bool)]
blocksOnlyWidthA blocks =
  foldl
    ( \accu x ->
        case x of
          (Nothing, _) -> accu
          (Just i, w) -> insert i [(w, True)] accu
    )
    empty
    (blocksOnlyWidth' blocks)

blocksAndGammaConnectionsWidthA' :: [Block] -> Map ID [(Double, Bool)] -> Map ID [(Double, Bool)]
blocksAndGammaConnectionsWidthA' blocks widths =
  foldl
    ( \accu block ->
        case block of
          (Fork _ _ l r (Just gCI)) ->
            let lWidths = blocksAndGammaConnectionsWidthA' l accu
                rWidths = blocksAndGammaConnectionsWidthA' r lWidths
             in case Data.Map.lookup gCI rWidths of
                  Nothing -> error $ show gCI <> " does not exist in the collection of widths: " <> show rWidths
                  Just [] -> error $ show gCI <> " does not contain any widths"
                  Just widths'@((width, _) : _) -> insert gCI ((width + 0.1, False) : widths') rWidths
          _ -> accu
    )
    widths
    blocks

blocksAndGammaConnectionsWidth' :: [Block] -> Map ID Double -> Map ID Double
blocksAndGammaConnectionsWidth' blocks widths =
  foldl
    ( \accu block ->
        case block of
          (Fork _ _ l r (Just gCI)) ->
            let lWidths = blocksAndGammaConnectionsWidth' l accu
                rWidths = blocksAndGammaConnectionsWidth' r lWidths
             in case Data.Map.lookup gCI rWidths of
                  Nothing -> error $ show gCI <> " does not exist in the collection of widths: " <> show rWidths
                  Just w' -> insert gCI (w' + 0.1) rWidths
          _ -> accu
    )
    widths
    blocks

blocksAndGammaConnectionsWidthA :: [Block] -> Map ID [(Double, Bool)]
blocksAndGammaConnectionsWidthA blocks =
  let newWidths = Blocks.blocksOnlyWidthA blocks
   in blocksAndGammaConnectionsWidthA' blocks newWidths

blocksAndGammaConnectionsWidth :: [Block] -> Map ID Double
blocksAndGammaConnectionsWidth blocks = blocksAndGammaConnectionsWidth' blocks (Blocks.blocksOnlyWidth blocks)

width1' :: Map ID Double -> [Block] -> Double
width1' gCs bs = case bs of
  [] -> defaultBoundingBoxWidth
  bs' -> maximum $ map (width1 gCs) bs'

width1 :: Map ID Double -> Block -> Double
width1 gCs (Fork i _ l r _) =
  let lW = width1' gCs l
      rW = width1' gCs r
      cW = case i of
        Nothing -> 0.0
        Just i' -> case Data.Map.lookup i' gCs of
          Nothing -> 0.0
          Just cW' -> cW'
   in lW + rW + cW
width1 _ _ = defaultBoundingBoxWidth

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
updateGammaConnections gammaConnectionOrigin i origins widths dGCs iGCs =
  case i of
    Nothing -> (dGCs, iGCs)
    Just i' -> case Data.Map.lookup i' origins of
      Nothing -> error $ show i' <> " does not exist in the collection of origins: " <> show origins
      Just gammaConnectionDestination -> case Data.Map.lookup i' widths of
        Nothing -> error $ show i' <> " does not exist in the collection of widths: " <> show widths
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

blockWidth :: Maybe ID -> Map ID Double -> Double
blockWidth Nothing _ = defaultBoundingBoxWidth
blockWidth (Just i) widths = case Data.Map.lookup i widths of
  Nothing -> 0.0
  Just w -> w

render' ::
  Block ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  Map ID Double ->
  [(Point V2 Double, Point V2 Double)] ->
  [[Point V2 Double]] ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
render' StartTerminator o@(P (V2 x _y)) _os widths _dGCs _iGCs = (position [(o, wyvernRoundedRect "start")], _os, _dGCs, _iGCs)
render' EndTerminator o@(P (V2 x _y)) _os widths _dGCs _iGCs = (position [(o, wyvernRoundedRect "end")], _os, _dGCs, _iGCs)
render' (Action i c) o@(P (V2 x _y)) os widths _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, _dGCs, _iGCs)
render' (Headline i c) o@(P (V2 x y)) os widths _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, _dGCs, _iGCs)
render' (Address i c) o@(P (V2 x y)) os widths _dGCs _iGCs = (position [(o, wyvernRect c)], updateOrigins i o os, _dGCs, _iGCs)
render' fork@(Fork i c l r gCId) o@(P (V2 x y)) origins widths dGCs iGCs =
  let lO = p2 (x, y - defaultBoundingBoxHeight)
      rO@(P (V2 r0x r0y)) = p2 (x + width l widths, y - defaultBoundingBoxHeight)
      origins' = updateOrigins i o origins
      (dGCs', iGCs') = updateGammaConnections (p2 (r0x, y - heightInUnits' r)) gCId origins' widths dGCs iGCs
      (renderedL, originsL, dGCsL, iGCsL) = render l lO origins' widths dGCs' iGCs'
      (renderedR, originsLR, dGCsLR, iGCsLR) = render r rO originsL widths dGCsL iGCsL
   in ( position [(p2 (x, y), wyvernRect c)]
          <> renderedL
          <> renderedR,
        originsLR,
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
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Point V2 Double)], [[Point V2 Double]])
render blocks o origins widths dGCs iGCs =
  let (diagram, _, origins', dGCs', iGCs') =
        foldl
          ( \(d, (P (V2 x y)), accuOrigins, accuDGCs, accuIGCs) block ->
              let h = height block
                  x' = x
                  y' = y - h
                  (d', accuOrigins', accuDGCs', accuIGCs') =
                    render' block (p2 (x, y)) accuOrigins widths accuDGCs accuIGCs
               in (d <> d', p2 (x', y'), accuOrigins', accuDGCs', accuIGCs')
          )
          (mempty, o, origins, dGCs, iGCs)
          blocks
   in (diagram, origins', dGCs', iGCs')

renderAll :: Diagram B -> [[Point V2 Double]] -> Diagram B
renderAll d iGCs = d <> foldl (\accu x -> accu <> renderedConnection x) mempty iGCs
