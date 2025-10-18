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

render' ::
  Block ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Point V2 Double)] ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Point V2 Double)])
render' StartTerminator o _os _gammaConnections = (position [(o, wyvernRoundedRect "start")], _os, _gammaConnections)
render' EndTerminator o _os _gammaConnections = (position [(o, wyvernRoundedRect "end")], _os, _gammaConnections)
render' (Action Nothing c) o _os _gammaConnections = (position [(o, wyvernRect c)], _os, _gammaConnections)
render' (Action (Just i) c) o os _gammaConnections = (position [(o, wyvernRect c)], insert i o os, _gammaConnections)
render' headline@(Headline Nothing c) o@(P (V2 x y)) _os _gammaConnections = (position [(o, wyvernRect c)], _os, _gammaConnections)
render' headline@(Headline (Just i) c) o@(P (V2 x y)) os _gammaConnections = (position [(o, wyvernRect c)], insert i o os, _gammaConnections)
render' (Address Nothing c) o _os _gammaConnections = (position [(o, wyvernRect c)], _os, _gammaConnections)
render' (Address (Just i) c) o os _gammaConnections = (position [(o, wyvernRect c)], insert i o os, _gammaConnections)
render' fork@(Fork i c l r gammaConnectionId) o@(P (V2 x y)) origins gammaConnections =
  let lO = p2 (x, y - defaultBoundingBoxHeight)
      -- 2025-10-08 PJ:
      -- --------------
      -- Fix.
      -- Actual: P (V2 10.0 2.0)
      -- Expected: P (V2 10.0 3.0)
      -- Or shift the coordinate system so that
      -- the end terminator is at 0.0.
      rO = p2 (x + width l, y - defaultBoundingBoxHeight)
      origins' = case i of
        Nothing -> origins
        Just i' -> insert i' o origins
      gammaConnections' = case gammaConnectionId of
        Nothing -> gammaConnections
        Just gammaConnectionId' -> case Data.Map.lookup gammaConnectionId' origins' of
          Nothing -> error $ show gammaConnectionId' <> " does not exist in the collection of origins: " <> show origins'
          Just gammaConnectionDestination ->
            let gammaConnectionOrigin = rO
             in (gammaConnectionOrigin, gammaConnectionDestination) : gammaConnections
      (renderedL, originsL, gammaConnectionsL) = render l lO origins' gammaConnections'
      (renderedR, originsLR, gammaConnectionsLR) = render r rO originsL gammaConnectionsL
   in ( position [(p2 (x, y), wyvernRect c)]
          <> renderedL
          <> renderedR,
        originsLR,
        gammaConnectionsLR
      )

render ::
  [Block] ->
  (Point V2 Double) ->
  Map ID (Point V2 Double) ->
  [(Point V2 Double, Point V2 Double)] ->
  (Diagram B, Map ID (Point V2 Double), [(Point V2 Double, Point V2 Double)])
render blocks o origins gammaConnections =
  let (diagram, _, origins', gammaConnections') =
        foldl
          ( \(d, (P (V2 x y)), accuOrigins, accuGammaConnections) block ->
              let h = height block
                  x' = x
                  y' = y - h
                  (d', accuOrigins', accuGammaConnections') =
                    render' block (p2 (x, y)) accuOrigins accuGammaConnections
               in (d <> d', p2 (x', y'), accuOrigins', accuGammaConnections')
          )
          (mempty, o, origins, gammaConnections)
          blocks
   in (diagram, origins', gammaConnections')
