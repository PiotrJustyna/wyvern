module Main where

import Blocks (renderDiagram, reverse)
import Constants (svgOptions)
import Diagrams.Backend.SVG (renderSVG')
import InputArguments (inputPath, outputPath, parseInput)
import Layout (connections, position, reposition)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import PositionedBlock (toMap)
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
                -- let positionedBlocks = position (Blocks.reverse validBlocks) 0.0 0.0
                -- let destinations = toMap positionedBlocks
                -- -- let (repositionedBlocks, _anyRepositioned) = reposition positionedBlocks (-10.0)
                -- -- print $ toMap repositionedBlocks
                -- -- let repositionedBlocks = positionedBlocks
                -- let blockConnections = connections positionedBlocks destinations
                -- let renderedBlocks = render positionedBlocks
                -- let renderedConnections = renderConnections blockConnections
                -- rendering v2:
                -- renderSVG' ((outputPath input) <> "_new") svgOptions (renderedBlocks <> renderedConnections)
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
