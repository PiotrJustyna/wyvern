module Renderer where

import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position)
import HelperDiagrams (renderConnection, wyvernAddress, wyvernHeadline, wyvernQuestion, wyvernQuestionV3, wyvernRect, wyvernRectV3, wyvernRoundedRect)
import PositionedBlock
import PositionedBlockV3

render'' :: PositionedBlock -> Diagram B
render'' pB@(PositionedFork _i _pId _c l r _gCId x _xR y _maxX _minYL _minYR _lLY _lRY) = position [((P (V2 x y)), wyvernQuestion $ show pB)] <> render' l <> render' r
render'' pB@(PositionedStartTerminator _pId x y _maxX _minY) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
render'' pB@(PositionedEndTerminator _pId x y _maxX _minY) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
render'' pB@(PositionedAction _i _pId _c x y _maxX _minY) = position [((P (V2 x y)), wyvernRect $ show pB)]
render'' pB@(PositionedHeadline _i _pId _c x y _maxX _minY) = position [((P (V2 x y)), wyvernHeadline $ show pB)]
render'' pB@(PositionedAddress _i _pId _c x y _maxX _minY) = position [((P (V2 x y)), wyvernAddress $ show pB)]

render' :: [PositionedBlock] -> Diagram B
render' [] = mempty
render' [pB] = render'' pB
render' (pB : pBs) = render'' pB <> render' pBs

render :: [[PositionedBlock]] -> Diagram B
render [] = mempty
render [skewer] = render' skewer
render (skewer : skewers) = render' skewer <> render skewers

renderV3'' :: PositionedBlockV3 -> Diagram B
renderV3'' pB@(PositionedForkV3 _i _pId _c l r _gCId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) =
  let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 pB
      boundingBoxWidth = maxX - x
      boundingBoxHeight = (minY - y) * (-1.0)
   in position [((P (V2 x y)), wyvernQuestionV3 (show pB) boundingBoxWidth boundingBoxHeight horizontalExpansion verticalBottomExpansion verticalTopExpansion)] <> renderV3' l <> renderV3' r
renderV3'' pB@(PositionedStartTerminatorV3 _pId x y _hExpansion _vExpansion) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
renderV3'' pB@(PositionedEndTerminatorV3 _pId x y _hExpansion _vExpansion) = position [((P (V2 x y)), wyvernRoundedRect $ show pB)]
renderV3'' pB@(PositionedActionV3 _i _pId _c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion) =
  let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion) = getPositionV3 pB
      boundingBoxWidth = maxX - x
      boundingBoxHeight = (minY - y) * (-1.0)
   in position [((P (V2 x y)), wyvernRectV3 (show pB) boundingBoxWidth boundingBoxHeight horizontalExpansion verticalBottomExpansion verticalTopExpansion)]
renderV3'' pB@(PositionedHeadlineV3 _i _pId _c x y _hExpansion _vExpansion) = position [((P (V2 x y)), wyvernHeadline $ show pB)]
renderV3'' pB@(PositionedAddressV3 _i _pId _c x y _hExpansion _vExpansion) = position [((P (V2 x y)), wyvernAddress $ show pB)]

renderV3' :: [PositionedBlockV3] -> Diagram B
renderV3' [] = mempty
renderV3' [pB] = renderV3'' pB
renderV3' (pB : pBs) = renderV3'' pB <> renderV3' pBs

renderV3 :: [[PositionedBlockV3]] -> Diagram B
renderV3 [] = mempty
renderV3 [skewer] = renderV3' skewer
renderV3 (skewer : skewers) = renderV3' skewer <> renderV3 skewers

renderConnections :: [((Double, Double), (Double, Double))] -> Diagram B
renderConnections = foldr (\((x1, y1), (x2, y2)) accu -> renderConnection [p2 (x1, y1), p2 (x2, y2)] <> accu) mempty
