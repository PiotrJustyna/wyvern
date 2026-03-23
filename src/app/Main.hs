module Main where

import Blocks (renderDiagram, validateBlocks)
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

  let lexingResult = runAlex fileContent lexAll

  case lexingResult of
    Left lexingError -> do
      putStrLn $ "Wyvern failed with the following error: " <> lexingError
    Right tokens -> do
      case diagram tokens 1 of
        ParseOk blocks -> do
          case validateBlocks blocks of
            Left validBlocks -> renderSVG' (outputPath input) svgOptions (Blocks.renderDiagram validBlocks)
            Right errors -> print errors
        ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
