module Main where

import Blocks (Block (Action, EndTerminator, Fork, StartTerminator), newRender, reverse')
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
import WyvernDiagram (WyvernDiagram' (..), newRender, newRender')

main :: IO ()
main = do
  input <- execParser options
  print input
  fileContent <- readFile $ inputPath input
  let tokens = alexScanTokens fileContent
  case diagram tokens 1 of
    ParseOk d -> do
      let d' = WyvernDiagram' (reverse' $ head d)
      -- let widths = WyvernDiagram.peek d'

      -- putStrLn "widths:"
      -- print widths

      -- let (rD, dGCs) = WyvernDiagram.renderAll d'

      -- putStrLn "direct gamma connections:"
      -- print dGCs

      -- let rD = WyvernDiagram.newRender d'
      let (rD, ds, gCs, w, h, maxW, maxH) = WyvernDiagram.newRender' d'

      putStrLn "gamma connections:"
      print gCs

      putStrLn "destinations:"
      print ds

      putStrLn "width:"
      print w

      putStrLn "height:"
      print h

      putStrLn "max width:"
      print maxW

      putStrLn "max height:"
      print maxH

      let rD' = WyvernDiagram.newRender d'

      renderSVG' (outputPath input) svgOptions rD'
    ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
