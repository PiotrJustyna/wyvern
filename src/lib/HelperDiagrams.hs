module HelperDiagrams where

import Constants (backgroundRectangleFillColour, defaultBoundingBoxHeight, defaultBoundingBoxWidth, defaultFontSize, fillColour, fillColourV3, fontColour, fontColourV3, heightRatio, lineColour, lineColourV3, smallFontSize, widthRatio, wyvernStyle, wyvernStyleV3)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude
  ( Diagram,
    Point (..),
    V2 (..),
    alignTL,
    alignY,
    closeLine,
    fc,
    font,
    fontSize,
    fromOffsets,
    fromVertices,
    lc,
    light,
    local,
    lw,
    none,
    opacity,
    p2,
    position,
    r2,
    rect,
    regPoly,
    rotateBy,
    roundedRect,
    scaleY,
    strokeLoop,
    text,
    translate,
    triangle,
    veryThin,
    (#),
  )

rect' :: Double -> Double -> Diagram B
rect' x y =
  fromOffsets [V2 x 0.0, V2 0.0 (y * (-1.0)), V2 (x * (-1.0)) 0.0, V2 0.0 y] # closeLine # strokeLoop # wyvernStyle

headlineShape :: Double -> Double -> Diagram B
headlineShape x y =
  fromOffsets [V2 x 0.0, V2 0.0 (y * (-1.0)), V2 (x * (-0.5)) (-0.1), V2 (x * (-0.5)) 0.1, V2 0.0 y]
    # closeLine
    # strokeLoop
    # wyvernStyle

addressShape :: Double -> Double -> Diagram B
addressShape x y =
  fromOffsets [V2 (x * 0.5) 0.1, V2 (x * 0.5) (-0.1), V2 0.0 (y * (-1.0)), V2 (x * (-1.0)) 0.0]
    # closeLine
    # strokeLoop
    # wyvernStyle

renderTextV3 :: String -> Diagram B
renderTextV3 x =
  text x
    # fontSize (local smallFontSize)
    # light
    # font "helvetica"
    # fc fontColourV3

renderText :: String -> Diagram B
renderText x =
  text x
    # fontSize (local defaultFontSize)
    # light
    # font "helvetica"
    # fc fontColour

wyvernRoundedRect :: String -> Diagram B
wyvernRoundedRect x =
  renderText x
    <> roundedRect (defaultBoundingBoxWidth * widthRatio) (defaultBoundingBoxHeight * heightRatio) 0.5 # lw veryThin # lc lineColour # fc fillColour

wyvernRectV3 :: String -> Double -> Double -> Double -> Double -> Double -> Diagram B
wyvernRectV3
  content
  boundingBoxWidth
  boundingBoxHeight
  horizontalExpansion
  verticalBottomExpansion
  verticalTopExpansion =
    renderTextV3 content # translate (r2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-0.5) - verticalTopExpansion))
      <> rect (defaultBoundingBoxWidth * widthRatio) (defaultBoundingBoxHeight * heightRatio) # wyvernStyleV3 # alignTL # translate (r2 (defaultBoundingBoxWidth * (1.0 - widthRatio) * 0.5, defaultBoundingBoxHeight * (1.0 - heightRatio) * (-0.5) - verticalTopExpansion))
      <> rect boundingBoxWidth boundingBoxHeight # lw none # fc backgroundRectangleFillColour # alignTL . opacity 0.1

wyvernRect :: String -> Diagram B
wyvernRect x =
  renderText x
    <> rect (defaultBoundingBoxWidth * widthRatio) (defaultBoundingBoxHeight * heightRatio) # lw veryThin # lc lineColour # fc fillColour

wyvernHeadline :: String -> Diagram B
wyvernHeadline x =
  renderText x
    <> fromOffsets
      [ V2 0.0 (defaultBoundingBoxHeight * heightRatio),
        V2 (defaultBoundingBoxWidth * widthRatio) 0.0,
        V2 0.0 (defaultBoundingBoxHeight * heightRatio * (-1.0)),
        V2 (defaultBoundingBoxWidth * widthRatio * (-0.5)) (-0.1),
        V2 (defaultBoundingBoxWidth * widthRatio * (-0.5)) 0.1
      ]
      # closeLine
      # strokeLoop
      # wyvernStyle
      # translate (r2 (defaultBoundingBoxWidth * widthRatio * (-0.5), defaultBoundingBoxHeight * heightRatio * (-0.5)))

wyvernAddress :: String -> Diagram B
wyvernAddress x =
  renderText x
    <> fromOffsets
      [ V2 0.0 (defaultBoundingBoxHeight * heightRatio),
        V2 (defaultBoundingBoxWidth * widthRatio * 0.5) 0.1,
        V2 (defaultBoundingBoxWidth * widthRatio * 0.5) (-0.1),
        V2 0.0 (defaultBoundingBoxHeight * heightRatio * (-1.0)),
        V2 (defaultBoundingBoxWidth * widthRatio * (-1.0)) 0.0
      ]
      # closeLine
      # strokeLoop
      # wyvernStyle
      # translate (r2 (defaultBoundingBoxWidth * widthRatio * (-0.5), defaultBoundingBoxHeight * heightRatio * (-0.5)))

wyvernQuestionV3 ::
  String ->
  Double ->
  Double ->
  Double ->
  Double ->
  Double ->
  Diagram B
wyvernQuestionV3
  c
  boundingBoxWidth
  boundingBoxHeight
  horizontalExpansion
  verticalBottomExpansion
  verticalTopExpansion =
    renderTextV3 c # translate (r2 (defaultBoundingBoxWidth * 0.5, (defaultBoundingBoxHeight * (-0.5) - verticalTopExpansion)))
      <> fromOffsets
        [ V2 (-0.1) (defaultBoundingBoxHeight * heightRatio * 0.5),
          V2 0.1 (defaultBoundingBoxHeight * heightRatio * 0.5),
          V2 (defaultBoundingBoxWidth * widthRatio - 0.2) 0.0,
          V2 0.1 (defaultBoundingBoxHeight * heightRatio * (-0.5)),
          V2 (-0.1) (defaultBoundingBoxHeight * heightRatio * (-0.5)),
          V2 (defaultBoundingBoxWidth * widthRatio * (-1.0) + 0.2) 0.0
        ]
        # closeLine
        # strokeLoop
        # wyvernStyleV3
        # alignTL
        # translate (r2 (defaultBoundingBoxWidth * (1.0 - widthRatio) * 0.5, defaultBoundingBoxHeight * heightRatio * (-0.5) - verticalTopExpansion))
      <> renderTextV3 "yes" # translate (r2 ((defaultBoundingBoxWidth * 0.5) - 0.2, defaultBoundingBoxHeight * (-0.85) - verticalTopExpansion))
      <> renderTextV3 "no" # translate (r2 (defaultBoundingBoxWidth * 0.95, defaultBoundingBoxHeight * (-0.4) - verticalTopExpansion))
      -- <> rect (boundingBoxWidth + horizontalExpansion) (boundingBoxHeight + verticalBottomExpansion + verticalTopExpansion)
      <> rect boundingBoxWidth boundingBoxHeight
        # fc backgroundRectangleFillColour
        # lw none
        # alignTL
        . opacity 0.1

wyvernQuestion :: String -> Diagram B
wyvernQuestion x =
  renderText x
    <> fromOffsets
      [ V2 (-0.1) (defaultBoundingBoxHeight * heightRatio * 0.5),
        V2 0.1 (defaultBoundingBoxHeight * heightRatio * 0.5),
        V2 (defaultBoundingBoxWidth * widthRatio - 0.2) 0.0,
        V2 0.1 (defaultBoundingBoxHeight * heightRatio * (-0.5)),
        V2 (-0.1) (defaultBoundingBoxHeight * heightRatio * (-0.5)),
        V2 (defaultBoundingBoxWidth * widthRatio * (-1.0) + 0.2) 0.0
      ]
      # closeLine
      # strokeLoop
      # wyvernStyle
      # translate (r2 (defaultBoundingBoxWidth * widthRatio * (-0.5) + 0.1, defaultBoundingBoxHeight * heightRatio * (-0.5)))
    <> renderText "yes" # translate (r2 (-0.2, defaultBoundingBoxHeight * (-0.4)))
    <> renderText "no" # translate (r2 (defaultBoundingBoxWidth * 0.45, 0.1))

wyvernHex :: String -> Diagram B
wyvernHex x =
  renderText x
    <> regPoly 6 ((defaultBoundingBoxWidth * widthRatio) / 2)
      # scaleY ((defaultBoundingBoxHeight * heightRatio) / (defaultBoundingBoxWidth * widthRatio))
      # wyvernStyle
    <> renderText "yes" # translate (r2 (-0.2, defaultBoundingBoxHeight * (-0.35)))
    <> renderText "no" # translate (r2 (defaultBoundingBoxWidth * 0.45, 0.1))

renderConnection :: [Point V2 Double] -> Diagram B
renderConnection coordinates = fromVertices coordinates # wyvernStyle

renderGammaConnection :: Point V2 Double -> Point V2 Double -> Double -> Double -> Diagram B
renderGammaConnection gO@(P (V2 gOX gOY)) gD@(P (V2 gDX gDY)) maxX minY =
  let gD'@(P (V2 gDX' gDY')) = p2 (gDX + (0.1 * sqrt 3.0 / 2.0) + 0.012, gDY + defaultBoundingBoxHeight * 0.5 - 0.1)
      midpointX =
        if gDX > gOX && maxX <= gDX
          then gDX + defaultBoundingBoxWidth * 0.5
          else maxX - defaultBoundingBoxWidth * 0.5
      gammaMidpoint1 = p2 (gOX, minY)
      gammaMidpoint2 = p2 (midpointX, minY)
      gammaMidpoint3 = p2 (midpointX, gDY')
      coordinates = [gO, gammaMidpoint1, gammaMidpoint2, gammaMidpoint3, gD']
   in renderConnection coordinates <> position [(p2 (gDX' - 0.025, gDY'), rotateBy (1 / 4) $ triangle 0.1 # wyvernStyle)]

renderUpperBetaConnections :: [(Double, Double)] -> Double -> Diagram B
renderUpperBetaConnections [] maxD = mempty
renderUpperBetaConnections [uBC@(uBCa, uBCb)] maxD =
  renderConnection
    [ p2 (uBCa, maxD + defaultBoundingBoxHeight * 0.5),
      p2 (uBCb, maxD + defaultBoundingBoxHeight * 0.5),
      p2 (uBCb, maxD + defaultBoundingBoxHeight * heightRatio * 0.5)
    ]
renderUpperBetaConnections ((uBCa, uBCb) : uBCs) maxD =
  renderConnection
    [ p2 (uBCa, maxD + defaultBoundingBoxHeight * 0.5),
      p2 (uBCb, maxD + defaultBoundingBoxHeight * 0.5),
      p2 (uBCb, maxD + defaultBoundingBoxHeight * heightRatio * 0.5)
    ]
    <> renderUpperBetaConnections uBCs maxD

renderSideBetaConnection :: Point V2 Double -> Point V2 Double -> Diagram B
renderSideBetaConnection a@(P (V2 aX aY)) (P (V2 bX bY)) =
  renderConnection
    [ a,
      p2 (aX - defaultBoundingBoxWidth * 0.5, aY),
      p2 (aX - defaultBoundingBoxWidth * 0.5, bY),
      p2 (bX - (0.1 * sqrt 3.0 / 2.0), bY)
    ]
    <> position [(p2 (bX - 0.06, bY), rotateBy (3 / 4) $ triangle 0.1 # wyvernStyle)]

renderLowerBetaConnections' :: [(Double, Double, Double)] -> Double -> Diagram B
renderLowerBetaConnections' [] _ = mempty
renderLowerBetaConnections' ((lBCa, lBCb, lBCc) : lBCs) minD =
  renderConnection [p2 (lBCa, lBCc + defaultBoundingBoxHeight), p2 (lBCa, minD), p2 (lBCb, minD)]
    <> renderLowerBetaConnections' lBCs minD

renderLowerBetaConnections :: [(Double, Double, Double)] -> Double -> Diagram B
renderLowerBetaConnections [] _ = mempty
renderLowerBetaConnections ((lBCa, _lBCb, lBCc) : lBCs) minD =
  renderConnection [p2 (lBCa, lBCc + defaultBoundingBoxHeight), p2 (lBCa, minD)]
    <> renderLowerBetaConnections' lBCs minD
