module Main where

import Blocks (renderDiagram)
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import InputArguments (inputPath, outputPath, parseInput)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import Validator (findDuplicatedIDs)

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
            print blocks
            let (_allIds, duplicatedIds) = findDuplicatedIDs blocks
            case duplicatedIds of
              [] -> do
                renderSVG' (outputPath input) svgOptions (Blocks.renderDiagram blocks)
                return 0
              _ -> do
                putStrLn $ "Block validation failed. Following IDs are duplicated: " <> show duplicatedIds
                return 1
          ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
