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

  let lexingResult = runAlex fileContent lexAll

  case lexingResult of
    Left lexingError -> do
      putStrLn $ "Wyvern failed with the following error: " <> lexingError
    Right tokens -> do
      case diagram tokens 1 of
        ParseOk d -> case Blocks.render d of
          Left rD -> renderSVG' (outputPath input) svgOptions rD
          Right errors -> print errors
        ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
