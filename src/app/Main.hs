module Main where

import Blocks (render)
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import InputArguments (inputPath, outputPath, parseInput)
-- import Lexer (alexScanTokens)
-- import LexerV2 (alexScanTokens)
import LexerV3 (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)

main :: IO ()
main = do
  input <- execParser options
  print input
  fileContent <- readFile $ inputPath input
  let tokens = runAlex fileContent lexAll
  putStrLn "tokens:"
  print tokens
  where
    -- let tokens = alexScanTokens fileContent
    -- case diagram tokens 1 of
    --   ParseOk d -> renderSVG' (outputPath input) svgOptions (Blocks.render d)
    --   ParseFail s -> error s

    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
