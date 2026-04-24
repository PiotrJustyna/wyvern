module Renderer where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), position)
import HelperDiagrams (wyvernAddress, wyvernHeadline, wyvernQuestion, wyvernRect, wyvernRoundedRect)
import PositionedBlock

render'' :: PositionedBlock -> Diagram B
render'' pB@(PositionedFork _i c l r _gCId x y) = position [((P (V2 x y)), wyvernQuestion $ show pB)] <> render' l <> render' r
render'' pB@(PositionedStartTerminator x y) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
render'' pB@(PositionedEndTerminator x y) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
render'' pB@(PositionedAction _i _c x y) = position [((P (V2 x y)), wyvernRect $ show pB)]
render'' pB@(PositionedHeadline _i _c x y) = position [((P (V2 x y)), wyvernHeadline $ show pB)]
render'' pB@(PositionedAddress _i _c x y) = position [((P (V2 x y)), wyvernAddress $ show pB)]

render' :: [PositionedBlock] -> Diagram B
render' [] = mempty
render' [pB] = render'' pB
render' (pB : pBs) = render'' pB <> render' pBs

render :: [[PositionedBlock]] -> Diagram B
render [] = mempty
render [skewer] = render' skewer
render (skewer : skewers) = render' skewer <> render skewers
