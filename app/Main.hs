module Main where

import Constants (defaultBoundingBoxHeight, svgOptions, svgOutputPath)
import Content (Content (Content))
import Diagrams.Backend.SVG (renderSVG')
import Diagrams.Prelude (p2)
import EndTerminator (EndTerminator (End))
import ID (ID (ID))
import Lexer (alexScanTokens)
import Parser (ParseResult (..), diagram)
import SkewerBlock (reverse'')
import StartTerminator (StartTerminator (Title))
import WyvernDiagram (WyvernDiagram (..), heightInUnits, render)

main :: IO ()
main = do
  fileContent <- readFile "./app/input.txt"
  let tokens = alexScanTokens fileContent
  case diagram tokens 1 of
    ParseOk d -> do
      print $ length d
      let blocks = reverse'' d
          wyvernDiagram =
            WyvernDiagram
              (Title (ID "-1") (p2 (-1.0, -1.0)) (Content "start"))
              blocks
              (End (ID "-1") (p2 (-1.0, -1.0)) (Content "end"))
          addressY = (-1.0) * WyvernDiagram.heightInUnits wyvernDiagram + defaultBoundingBoxHeight * 2.0 -- ignore the heights of start and end terminators
      renderSVG' svgOutputPath svgOptions $ WyvernDiagram.render wyvernDiagram addressY
    ParseFail s -> error s
