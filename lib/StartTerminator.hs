module StartTerminator where

import Constants
import Content (Content (..))
import Data.Map (Map)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point, V2 (..), position, r2, roundedRect, translate, (#))
import HelperDiagrams
import ID (ID)

data StartTerminator
  = Title ID (Point V2 Double) Content
  deriving (Show)

changeOrigin :: StartTerminator -> Point V2 Double -> StartTerminator
changeOrigin (Title startId _ content) newOrigin = Title startId newOrigin content

render :: StartTerminator -> Diagram B
render title@(Title titleId origin (Content content)) =
  position
    [ ( origin,
        renderText
          ( ( if troubleshootingMode
                then "[" <> show titleId <> " | " <> show origin <> "] "
                else ""
            )
              <> content
          )
          (0.0 + widthInUnits title * defaultBoundingBoxWidth * 0.5)
          (0.0 - heightInUnits title * defaultBoundingBoxHeight * 0.5)
          <> ( drakonStyle
                 ( roundedRect
                     (widthInUnits title * defaultBoundingBoxWidth * widthRatio)
                     (heightInUnits title * defaultBoundingBoxHeight * 0.5)
                     0.5
                 )
                 # translate (r2 (defaultBoundingBoxWidth * 0.5, defaultBoundingBoxHeight * (-0.5)))
             )
          <> if troubleshootingMode
            then
              boundingBox
                (widthInUnits title * defaultBoundingBoxWidth)
                (heightInUnits title * defaultBoundingBoxHeight)
            else mempty
      )
    ]
render _ = mempty

widthInUnits :: StartTerminator -> Double
widthInUnits _ = 1.0

heightInUnits :: StartTerminator -> Double
heightInUnits _ = 1.0
