module Renderer where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), position)
import HelperDiagrams (wyvernRect)
import PositionedBlock

render' :: PositionedBlock -> Diagram B
render' pB =
  let (x, y) = getPosition pB
   in position [((P (V2 x y)), wyvernRect $ "-")]

render :: [PositionedBlock] -> Diagram B
render [] = mempty
render [pB] = render' pB
render (pB : pBs) = render' pB <> render pBs
