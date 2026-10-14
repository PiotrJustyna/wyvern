module Main where

import Blocks (renderDiagram, reverse)
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import ID
import InputArguments (inputPath, outputPath, parseInput)
import IntermediateBlock
import Layout (expand1, positionV3, repositionV3)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import PositionedBlockV3 (originsAndDestinations, position, positionedBlocksById)
import Renderer (renderGammaConnections, renderV3)
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
                -- let positionedBlocksV3 = positionV3 (Blocks.reverse validBlocks) 0 0.0 0.0
                -- let expandedBlocksV3 = expand1 positionedBlocksV3 9 0.5 0.5 0.0
                -- let expandedBlocksV3' = repositionV3 expandedBlocksV3 0.0 0.0
                -- let expandedBlocksV3'' = expand1 expandedBlocksV3' 7 0.0 0.0 0.5
                -- let expandedBlocksV3''' = repositionV3 expandedBlocksV3'' 0.0 0.0

                -- rendering v3:
                -- renderSVG' ((outputPath input) <> "_v3") svgOptions ((render repositionedBlocks) <> renderedConnections3)
                -- renderSVG' ((outputPath input) <> "_v3") svgOptions (renderV3 expandedBlocksV3''')

                let (intermediateBlocks, gammaConnections) = fromBlocks validBlocks
                --                 putStrLn "intermediate blocks:"
                --                 putStrLn $ showBlocks intermediateBlocks
                --
                putStrLn "gamma connections:"
                putStrLn . show $ gammaConnections

                let expandedIntermediateBlocks = gammaExpansions intermediateBlocks gammaConnections
                -- putStrLn "intermediate blocks expanded for gamma connections:"
                -- putStrLn $ showBlocks expandedIntermediateBlocks

                let positionedBlocks = position expandedIntermediateBlocks 0.0 0.0
                -- putStrLn "positoned blocks:"
                -- print positionedBlocks

                let (groupedByUserId, groupedByProgrammaticId) = positionedBlocksById positionedBlocks
                -- putStrLn "positioned blocks grouped by user id:"
                -- print groupedByUserId
                -- putStrLn "positioned blocks grouped by programmatic id:"
                -- print groupedByProgrammaticId

                let (gammaCoordinates, gammaExpansions) = originsAndDestinations (groupedByUserId, groupedByProgrammaticId) gammaConnections
                putStrLn "gamma coordinates:"
                print gammaCoordinates
                putStrLn "gamma expansions:"
                print gammaExpansions

                -- rendering v3:
                renderSVG' ((outputPath input) <> "_v3") svgOptions ((renderV3 positionedBlocks) <> renderGammaConnections gammaCoordinates)

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
