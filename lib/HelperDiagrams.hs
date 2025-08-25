module HelperDiagrams where

import Constants (defaultFontSize, drakonStyle, fontColour, lineColour)
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
    r2,
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

renderedConnection :: [Point V2 Double] -> Diagram B
renderedConnection coordinates = fromVertices coordinates # drakonStyle
