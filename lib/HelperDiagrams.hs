module HelperDiagrams where

import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth, defaultFontSize, drakonStyle, fillColour, fontColour, heightRatio, lineColour, widthRatio)
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
    r2,
    rect,
    roundedRect,
    strokeLoop,
    text,
    translate,
    veryThin,
    (#),
  )

rect' :: Double -> Double -> Diagram B
rect' x y =
  fromOffsets [V2 x 0.0, V2 0.0 (y * (-1.0)), V2 (x * (-1.0)) 0.0, V2 0.0 y] # closeLine # strokeLoop # drakonStyle

headlineShape :: Double -> Double -> Diagram B
headlineShape x y =
  fromOffsets [V2 x 0.0, V2 0.0 (y * (-1.0)), V2 (x * (-0.5)) (-0.1), V2 (x * (-0.5)) 0.1, V2 0.0 y]
    # closeLine
    # strokeLoop
    # drakonStyle

addressShape :: Double -> Double -> Diagram B
addressShape x y =
  fromOffsets [V2 (x * 0.5) 0.1, V2 (x * 0.5) (-0.1), V2 0.0 (y * (-1.0)), V2 (x * (-1.0)) 0.0]
    # closeLine
    # strokeLoop
    # drakonStyle

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
    [ V2 (x - 0.1 - 0.1) 0.0,
      V2 0.1 (y * (-0.5)),
      V2 (-0.1) (y * (-0.5)),
      V2 ((x - 0.1 - 0.1) * (-1.0)) 0.0,
      V2 (-0.1) (y * 0.5),
      V2 0.1 (y * 0.5)
    ]
    # closeLine
    # strokeLoop
    # drakonStyle

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
        p2 (0.0, (h * (-0.5) - 0.1)),
        p2 (w * (-0.5), h * (-0.5))
      ]
      # closeLine
      # strokeLoop
      # lw veryThin
      # lc lineColour
      # fc fillColour

renderedConnection :: [Point V2 Double] -> Diagram B
renderedConnection coordinates = fromVertices coordinates # drakonStyle
