module Main where

import Blocks (renderDiagram, validateBlocks)
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import InputArguments (inputPath, outputPath, parseInput)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)

main :: IO Int
main = do
  input <- execParser options
  print input
  fileContent <- readFile $ inputPath input

  let lexingResult = runAlex fileContent lexAll

  case lexingResult of
    Left lexingError ->
      do
        putStrLn $ "Wyvern failed with the following error: " <> lexingError
        return 1
    Right tokens ->
      do
        case diagram tokens 1 of
          ParseOk blocks -> do
            case validateBlocks blocks of
              Left validBlocks ->
                do
                  renderSVG' (outputPath input) svgOptions (Blocks.renderDiagram validBlocks)
                  return 0
              Right (errors, ids) ->
                do
                  putStrLn "Diagram validation failed with the following error(s):"
                  print errors
                  putStrLn "Available block identifiers to be used as gamma connections destinations:"
                  print ids
                  return 1
          ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
