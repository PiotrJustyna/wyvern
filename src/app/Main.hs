module Main where

import Blocks (renderDiagram, reverse)
import Constants (repositionShift, svgOptions)
import Data.Map (empty, insert, lookup)
import Diagrams.Backend.SVG (renderSVG')
import ID
import InputArguments (inputPath, outputPath, parseInput)
import Layout (anotherReposition, barebonesGamma, buildGammaConnection, connectionsV2, expand1, position, positionV3, repositionBottomY, repositionOrigins, repositionOriginsTopY, repositionTopY, repositionV3, repositionX)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import PositionedBlock (PositionedBlock (..), extractGammaConnections, getMaxNumberOfShiftsPerDepth, qwe, toMap, toMapMicro)
import Renderer (render, renderConnections, renderV3)
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
                -- let positionedBlocks = position (Blocks.reverse validBlocks) 0 0.0 0.0
                -- let positionedBlocksV3 = positionV3 (Blocks.reverse validBlocks) 0 0.0 0.0
                -- let expandedBlocksV3 = expand1 positionedBlocksV3 9 0.0 0.0 0.0
                -- let expandedBlocksV3' = repositionV3 expandedBlocksV3 0.0 0.0
                -- let expandedBlocksV3'' = expand1 expandedBlocksV3' 9 0.5 0.5 0.5
                -- let expandedBlocksV3''' = repositionV3 expandedBlocksV3'' 0.0 0.0

                -- rendering v3:
                -- renderSVG' ((outputPath input) <> "_v3") svgOptions ((render repositionedBlocks) <> renderedConnections3)
                -- renderSVG' ((outputPath input) <> "_v3") svgOptions (renderV3 expandedBlocksV3''')

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

-- 2026-08-31 PJ:
-- ==============
-- Let this idea go: ([((Double, Double), ID, Double)], [[PositionedBlock]]).
-- Replace with: [[PositionedBlock]]
processGammaShifts ::
  [((Int, Double, Double), ID, Double)] ->
  [[PositionedBlock]] ->
  [[PositionedBlock]]
processGammaShifts [] positionedBlocks = positionedBlocks
processGammaShifts (g@((_pId, xOrigin, yOrigin), gCId, maxXOrigin) : gs) positionedBlocks =
  let destinations = toMap positionedBlocks
      (repositionedBlocks, gs') = case Data.Map.lookup gCId destinations of
        Nothing -> (positionedBlocks, gs)
        Just (_dX, dY, _dMaxX, _dMinY, _gammaDShiftX, _gammaDShiftY) ->
          ( repositionX maxXOrigin (repositionTopY dY (repositionBottomY yOrigin positionedBlocks)),
            repositionOriginsTopY dY (repositionOrigins maxXOrigin yOrigin gs)
          )
   in processGammaShifts gs' repositionedBlocks
