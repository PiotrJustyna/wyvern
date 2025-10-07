module Blocks where

import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, heightRatio, widthRatio)
import Content (Content (..))
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

render' :: Block -> (Point V2 Double) -> Diagram B
render' StartTerminator o = position [(o, wyvernRoundedRect "start")]
render' (Action _ c) o = position [(o, wyvernRect c)]
render' headline@(Headline _ c) o@(P (V2 x y)) = position [(o, wyvernRect c)]
render' (Address _ c) o = position [(o, wyvernRect c)]
render' fork@(Fork _ c l r _) o@(P (V2 x y)) =
  position [(o, wyvernRect c)]
    <> render l (p2 (x, y - height fork))
    <> render r (p2 (x + width l, y - height fork))
render' EndTerminator o = position [(o, wyvernRoundedRect "end")]

render :: [Block] -> (Point V2 Double) -> Diagram B
render bs o =
  let (diagram, _) =
        foldr
          ( \b (d, (P (V2 x y))) ->
              let h = height b
                  x' = x
                  y' = y + h
                  d' = render' b (p2 (x', y'))
               in (d <> d', p2 (x', y'))
          )
          (mempty, o)
          bs
   in diagram
