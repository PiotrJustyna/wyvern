module HelperDiagrams where

import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, defaultFontSize, fillColour, fontColour, heightRatio, lineColour, widthRatio, wyvernStyle)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude
  ( Diagram,
    Point (..),
    V2 (..),
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

boundingBox :: Double -> Double -> Diagram B
boundingBox x y =
  fromOffsets [V2 x 0.0, V2 0.0 (y * (-1.0)), V2 (x * (-1.0)) 0.0, V2 0.0 y]
    # closeLine
    # strokeLoop
    # lw veryThin
    # lc lineColour

hex' :: Double -> Double -> Diagram B
hex' x y =
  fromOffsets
    [ V2 (defaultBoundingBoxWidth * widthRatio - 0.1 - 0.1 - 1.5) 0.0,
      V2 0.1 (defaultBoundingBoxHeight * heightRatio * (-0.5)),
      V2 (-0.1) (defaultBoundingBoxHeight * heightRatio * (-0.5)),
      V2 ((defaultBoundingBoxWidth * widthRatio - 0.1 - 0.1) * (-1.0)) 0.0,
      V2 (-0.1) (defaultBoundingBoxHeight * heightRatio * 0.5),
      V2 0.1 (defaultBoundingBoxHeight * heightRatio * 0.5)
    ]
    # closeLine
    # strokeLoop
    # wyvernStyle

renderText :: String -> Double -> Double -> Diagram B
renderText content translateX translateY =
  text content
    # fontSize (local defaultFontSize)
    # light
    # font "helvetica"
    # fc fontColour
    # translate (r2 (translateX, translateY))

renderText' :: String -> Diagram B
renderText' x =
  text x
    # fontSize (local defaultFontSize)
    # light
    # font "helvetica"
    # fc fontColour

wyvernRoundedRect :: String -> Diagram B
wyvernRoundedRect x =
  renderText' x
    <> roundedRect (defaultBoundingBoxWidth * widthRatio) (defaultBoundingBoxHeight * heightRatio) 0.5 # lw veryThin # lc lineColour # fc fillColour

wyvernRect :: String -> Diagram B
wyvernRect x =
  renderText' x
    <> rect (defaultBoundingBoxWidth * widthRatio) (defaultBoundingBoxHeight * heightRatio) # lw veryThin # lc lineColour # fc fillColour

wyvernHeadline :: String -> Double -> Double -> Diagram B
wyvernHeadline x w h =
  renderText' x
    <> fromVertices
      [ p2 (w * (-0.5), h * 0.5),
        p2 (w * 0.5, h * 0.5),
        p2 (w * 0.5, h * (-0.5)),
        p2 (0.0, h * (-0.5) - 0.1),
        p2 (w * (-0.5), h * (-0.5))
      ]
      # closeLine
      # strokeLoop
      # lw veryThin
      # lc lineColour
      # fc fillColour

wyvernHex :: String -> Diagram B
wyvernHex x =
  renderText' x
    <> regPoly 6 ((defaultBoundingBoxWidth * widthRatio) / 2)
      # scaleY ((defaultBoundingBoxHeight * heightRatio) / (defaultBoundingBoxWidth * widthRatio))
      # wyvernStyle
    <> renderText' "yes" # translate (r2 (-0.2, defaultBoundingBoxHeight * (-0.35)))
    <> renderText' "no" # translate (r2 (defaultBoundingBoxWidth * 0.45, 0.1))

renderConnection :: [Point V2 Double] -> Diagram B
renderConnection coordinates = fromVertices coordinates # wyvernStyle

renderGammaConnection :: Point V2 Double -> Point V2 Double -> Double -> Double -> Diagram B
renderGammaConnection gO@(P (V2 gOX gOY)) gD@(P (V2 gDX gDY)) maxX minY =
  let gD'@(P (V2 gDX' gDY')) = p2 (gDX + (0.1 * sqrt 3.0 / 2.0) + 0.012, gDY + defaultBoundingBoxHeight * 0.5)
      gammaMidpoint1 = p2 (gOX, minY)
      gammaMidpoint2 = p2 (maxX - defaultBoundingBoxWidth * 0.5, minY)
      gammaMidpoint3 = p2 (maxX - defaultBoundingBoxWidth * 0.5, gDY')
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
renderUpperBetaConnections (uBC@(uBCa, uBCb) : uBCs) maxD =
  renderConnection
    [ p2 (uBCa, maxD + defaultBoundingBoxHeight * 0.5),
      p2 (uBCb, maxD + defaultBoundingBoxHeight * 0.5),
      p2 (uBCb, maxD + defaultBoundingBoxHeight * heightRatio * 0.5)
    ]
    <> renderUpperBetaConnections uBCs maxD

renderSideBetaConnection :: Point V2 Double -> Point V2 Double -> Diagram B
renderSideBetaConnection a@(P (V2 aX aY)) b@(P (V2 bX bY)) =
  renderConnection
    [ a,
      p2 (aX - defaultBoundingBoxWidth * 0.5, aY),
      p2 (aX - defaultBoundingBoxWidth * 0.5, bY),
      p2 (bX - (0.1 * sqrt 3.0 / 2.0), bY)
    ]
    <> position [(p2 (bX - 0.06, bY), rotateBy (3 / 4) $ triangle 0.1 # wyvernStyle)]

renderLowerBetaConnections' :: [(Double, Double, Double)] -> Double -> Diagram B
renderLowerBetaConnections' [] _ = mempty
renderLowerBetaConnections' (lBC@(lBCa, lBCb, lBCc) : lBCs) minD =
  renderConnection [p2 (lBCa, lBCc + defaultBoundingBoxHeight), p2 (lBCa, minD), p2 (lBCb, minD)]
    <> renderLowerBetaConnections' lBCs minD

renderLowerBetaConnections :: [(Double, Double, Double)] -> Double -> Diagram B
renderLowerBetaConnections [] _ = mempty
renderLowerBetaConnections (lBC@(lBCa, lBCb, lBCc) : lBCs) minD =
  renderConnection [p2 (lBCa, lBCc + defaultBoundingBoxHeight), p2 (lBCa, minD)]
    <> renderLowerBetaConnections' lBCs minD
