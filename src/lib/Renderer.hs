module Renderer where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position)
import HelperDiagrams (renderConnection, wyvernAddress, wyvernHeadline, wyvernQuestion, wyvernRect, wyvernRoundedRect)
import PositionedBlock

render'' :: PositionedBlock -> Diagram B
render'' pB@(PositionedFork _i _c l r _gCId x y _maxX _minY) = position [((P (V2 x y)), wyvernQuestion $ show pB)] <> render' l <> render' r
render'' pB@(PositionedStartTerminator x y _maxX _minY) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
render'' pB@(PositionedEndTerminator x y _maxX _minY) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
render'' pB@(PositionedAction _i _c x y _maxX _minY) = position [((P (V2 x y)), wyvernRect $ show pB)]
render'' pB@(PositionedHeadline _i _c x y _maxX _minY) = position [((P (V2 x y)), wyvernHeadline $ show pB)]
render'' pB@(PositionedAddress _i _c x y _maxX _minY) = position [((P (V2 x y)), wyvernAddress $ show pB)]

render' :: [PositionedBlock] -> Diagram B
render' [] = mempty
render' [pB] = render'' pB
render' (pB : pBs) = render'' pB <> render' pBs

render :: [[PositionedBlock]] -> Diagram B
render [] = mempty
render [skewer] = render' skewer
render (skewer : skewers) = render' skewer <> render skewers

renderConnections :: [((Double, Double), (Double, Double))] -> Diagram B
renderConnections = foldr (\((x1, y1), (x2, y2)) accu -> renderConnection [p2 (x1, y1), p2 (x2, y2)] <> accu) mempty
