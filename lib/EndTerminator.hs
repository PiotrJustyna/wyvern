module EndTerminator where

import Constants
  ( defaultBoundingBoxHeight,
    defaultBoundingBoxWidth,
    drakonStyle,
    troubleshootingMode,
    widthRatio,
  )
import Content (Content (..))
import Data.Map (Map)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), position, r2, roundedRect, translate, (#))
import HelperDiagrams (boundingBox, renderText)
import ID (ID)

data EndTerminator
  = End ID (Point V2 Double) Content
  deriving (Show)

changeOrigin :: EndTerminator -> Point V2 Double -> EndTerminator
changeOrigin (End endId _ content) newOrigin = End endId newOrigin content

render :: EndTerminator -> Diagram B
render end@(End endId origin (Content content)) =
  position
    [ ( origin,
        renderText
          ( ( if troubleshootingMode
                then "[" <> show endId <> " | " <> show origin <> "] "
                else ""
            )
              <> content
          )
          (0.0 + widthInUnits end * defaultBoundingBoxWidth * 0.5)
          (0.0 - heightInUnits end * defaultBoundingBoxHeight * 0.5)
          <> ( drakonStyle
                 ( roundedRect
                     (widthInUnits end * defaultBoundingBoxWidth * widthRatio)
                     (heightInUnits end * defaultBoundingBoxHeight * 0.5)
                     0.5
                 )
                 # translate (r2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-0.5)))
             )
          <> if troubleshootingMode
            then
              boundingBox
                (widthInUnits end * defaultBoundingBoxWidth)
                (heightInUnits end * defaultBoundingBoxHeight)
            else mempty
      )
    ]

widthInUnits :: EndTerminator -> Double
widthInUnits _ = 1.0

heightInUnits :: EndTerminator -> Double
heightInUnits _ = 1.0
