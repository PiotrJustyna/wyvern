module Main where

import Blocks (Block (Action, EndTerminator, Fork, StartTerminator), reverse')
import Constants (defaultBoundingBoxHeight, svgOptions)
import Content (Content (Content))
import Diagrams.Backend.SVG (renderSVG')
import Diagrams.Prelude (p2)
import ID (ID (ID))
import InputArguments
  ( inputPath,
    outputPath,
    parseInput,
  )
import Lexer (alexScanTokens)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import SkewerBlock (reverse'')
import WyvernDiagram (WyvernDiagram' (..), peek, renderAll)

main :: IO ()
main = do
  input <- execParser options
  print input
  fileContent <- readFile $ inputPath input
  let tokens = alexScanTokens fileContent
  case diagram tokens 1 of
    ParseOk d -> do
      let d' = WyvernDiagram' (reverse' $ head d)
      let (os, ds) = WyvernDiagram.peek d'

      putStrLn "dimensions:"
      print ds

      let rD = WyvernDiagram.renderAll d'
      renderSVG' (outputPath input) svgOptions rD
    ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
