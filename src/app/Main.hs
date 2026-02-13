module Main where

import Blocks (render)
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import InputArguments (inputPath, outputPath, parseInput)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)

main :: IO ()
main = do
  input <- execParser options
  print input
  fileContent <- readFile $ inputPath input

  -- lexer v1/v2
  -- let tokens = alexScanTokens fileContent

  -- putStrLn "tokens:"
  -- print tokens

  -- lexer v3
  let lexingResult = runAlex fileContent lexAll

  case lexingResult of
    Left lexingError -> do
      putStrLn $ "Wyvern failed with the following error: " <> lexingError
    Right tokens -> do
      -- print tokens
      case diagram tokens 1 of
        ParseOk d -> renderSVG' (outputPath input) svgOptions (Blocks.render d)
        ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
