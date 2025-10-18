module Main where

import Blocks (Block (Action, EndTerminator, Fork, StartTerminator), reverse')
import Constants (defaultBoundingBoxHeight, svgOptions)
import Content (Content (Content))
import Diagrams.Backend.SVG (renderSVG')
import Diagrams.Prelude (p2)
import EndTerminator (EndTerminator (End))
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
import StartTerminator (StartTerminator (Title))
import WyvernDiagram (WyvernDiagram (..), WyvernDiagram' (..), heightInUnits, render, render')

main :: IO ()
main = do
  input <- execParser options
  print input
  fileContent <- readFile $ inputPath input
  let tokens = alexScanTokens fileContent
  case diagram tokens 1 of
    ParseOk d -> do
      let (renderedDiagram, origins, connections) = WyvernDiagram.render' (WyvernDiagram' (reverse' $ head d))
      renderSVG' (outputPath input) svgOptions renderedDiagram
      putStrLn "origins:"
      print origins
      putStrLn "connections:"
      print connections
    ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
