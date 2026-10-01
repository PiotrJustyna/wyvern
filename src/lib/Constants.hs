module Constants where

import Data.Colour (AlphaColour, withOpacity)
import Data.Colour.SRGB (sRGB)
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
import Diagrams.Prelude (Colour, Diagram, V2 (..), fc, lc, lw, mkSizeSpec, opacity, veryThin, (#))

defaultBoundingBoxWidth :: Double
defaultBoundingBoxWidth = 3.0

defaultBoundingBoxHeight :: Double
defaultBoundingBoxHeight = 1.0

widthRatio :: Double
widthRatio = 0.8

heightRatio :: Double
heightRatio = 0.5

repositionShift :: Double
repositionShift = 0.5

-- colours used:
-- https://www.colourlovers.com/palette/541086/Loyal_Friends
-- https://www.colourlovers.com/palette/292482/Terra
lineColour :: Colour Double
lineColour = sRGB (160.0 / 255.0) (194.0 / 255.0) (222.0 / 255.0)

lineColourV3 :: Colour Double
lineColourV3 = sRGB (3.0 / 255.0) (22.0 / 255.0) (52.0 / 255.0)

backgroundRectangleFillColour :: Colour Double
backgroundRectangleFillColour = sRGB (3.0 / 255.0) (101.0 / 255.0) (100.0 / 255.0)

fillColourV3 :: Colour Double
fillColourV3 = sRGB (3.0 / 255.0) (54.0 / 255.0) (73.0 / 255.0)

fillColour :: Colour Double
fillColour = sRGB (237.0 / 255.0) (237.0 / 255.0) (244.0 / 255.0)

fontColourV3 :: Colour Double
fontColourV3 = sRGB (205.0 / 255.0) (179.0 / 255.0) (128.0 / 255.0)

fontColour :: Colour Double
fontColour = sRGB (6.0 / 255.0) (71.0 / 255.0) (128.0 / 255.0)

troubleshootingMode :: Bool
troubleshootingMode = False

smallFontSize :: Double
smallFontSize = defaultBoundingBoxHeight / 10.0

defaultFontSize :: Double
defaultFontSize = defaultBoundingBoxHeight / 8.0

wyvernStyle :: Diagram B -> Diagram B
wyvernStyle = lw veryThin # lc lineColour # fc fillColour

wyvernStyleV3 :: Diagram B -> Diagram B
wyvernStyleV3 = lw veryThin # lc lineColourV3 # fc fillColourV3

svgOptions :: (Num n) => Options SVG V2 n
svgOptions =
  SVGOptions
    { _size = mkSizeSpec $ V2 (Just 1920) (Just 1080),
      _svgDefinitions = Nothing,
      _svgAttributes = [],
      _generateDoctype = True
    }
