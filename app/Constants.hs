module Constants where

import Data.Colour.SRGB (sRGB)
import Data.Text (empty)
import Diagrams.Backend.SVG
  ( B,
    Options (SVGOptions),
    SVG,
    _generateDoctype,
    _idPrefix,
    _size,
    _svgAttributes,
    _svgDefinitions,
  )
import Diagrams.Prelude (Colour, Diagram, V2 (..), fc, lc, lw, mkSizeSpec, veryThin, (#))

defaultBoundingBoxWidth :: Double
defaultBoundingBoxWidth = 3.0

defaultBoundingBoxHeight :: Double
defaultBoundingBoxHeight = 1.0

widthRatio :: Double
widthRatio = 0.8

-- colours used:
-- https://www.colourlovers.com/palette/541086/Loyal_Friends
lineColour :: Colour Double
lineColour = sRGB (160.0 / 255.0) (194.0 / 255.0) (222.0 / 255.0)

fillColour :: Colour Double
fillColour = sRGB (237.0 / 255.0) (237.0 / 255.0) (244.0 / 255.0)

fontColour :: Colour Double
fontColour = sRGB (6.0 / 255.0) (71.0 / 255.0) (128.0 / 255.0)

troubleshootingMode :: Bool
troubleshootingMode = False

defaultFontSize :: Double
defaultFontSize = defaultBoundingBoxHeight / 8.0

drakonStyle :: Diagram B -> Diagram B
drakonStyle = lw veryThin # lc lineColour # fc fillColour

svgOptions :: (Num n) => Options SVG V2 n
svgOptions =
  SVGOptions
    { _size = mkSizeSpec $ V2 (Just 300) (Just 1000),
      _idPrefix = empty,
      _svgDefinitions = Nothing,
      _svgAttributes = [],
      _generateDoctype = True
    }
