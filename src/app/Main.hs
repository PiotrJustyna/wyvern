module Main where

import Blocks (reverse')
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import InputArguments
  ( inputPath,
    outputPath,
    parseInput,
  )
import Lexer (alexScanTokens)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import WyvernDiagram (WyvernDiagram' (..), newRender1)

main :: IO ()
main = do
  input <- execParser options
  print input
  fileContent <- readFile $ inputPath input
  let tokens = alexScanTokens fileContent
  case diagram tokens 1 of
    ParseOk d -> do
      let d' = Blocks.reverse' d
      let d'' = WyvernDiagram' d'
      let rD' = WyvernDiagram.newRender1 d''
      renderSVG' (outputPath input) svgOptions rD'
    ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
