module Main where

import Blocks (renderDiagram, reverse)
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import InputArguments (inputPath, outputPath, parseInput)
import Layout (connections, position, repositionBasedOnGamma)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import PositionedBlock (extractGammaConnections, getMaxNumberOfShiftsPerDepth, qwe, toMap, toMapMicro)
import Renderer (render, renderConnections)
import Validator (validate)

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
            case validate blocks of
              Left validBlocks -> do
                let positionedBlocks = position (Blocks.reverse validBlocks) 0.0 0.0
                let destinationsMicro = toMapMicro positionedBlocks
                -- TODO:
                -- Probably a good idea to preserve those steps for debugging just in case.
                let gammaConnections = extractGammaConnections destinationsMicro positionedBlocks
                print gammaConnections

                let defInput = qwe gammaConnections
                print defInput

                let maxNumberOfShiftsPerDepth = getMaxNumberOfShiftsPerDepth defInput
                print maxNumberOfShiftsPerDepth

                let repositionedBlocks = repositionBasedOnGamma positionedBlocks maxNumberOfShiftsPerDepth
                putStrLn "positionedBlocks:"
                print positionedBlocks
                putStrLn "repositionedBlocks:"
                print repositionedBlocks

                let destinations = toMap repositionedBlocks

                let blockConnections = connections repositionedBlocks destinations
                let renderedConnections = renderConnections blockConnections

                -- rendering v2:
                -- renderSVG' ((outputPath input) <> "_new") svgOptions ((render repositionedBlocks) <> renderedConnections)

                -- rendering v1:
                renderSVG' (outputPath input) svgOptions (Blocks.renderDiagram validBlocks)
                return 0
              Right (duplicatedIds, incorrectGCIds) -> do
                putStrLn "Block validation failed."
                putStrLn $ "*\tFollowing IDs are duplicated: " <> show duplicatedIds
                putStrLn $ "*\tFollowing gamma connection IDs are not correct: " <> show incorrectGCIds
                return 1
          ParseFail s -> error s
  where
    options =
      info
        (parseInput <**> helper)
        (fullDesc <> header "Wyvern")
