module Main where

import Blocks (renderDiagram, reverse)
import Constants (repositionShift, svgOptions)
import Data.Map (empty, insert, lookup)
import Diagrams.Backend.SVG (renderSVG')
import ID
import InputArguments (inputPath, outputPath, parseInput)
import Layout (barebonesGamma, buildGammaConnection, connectionsV2, position, repositionBottomY, repositionOrigins, repositionOriginsTopY, repositionTopY, repositionX)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import PositionedBlock (PositionedBlock (..), extractGammaConnections, getMaxNumberOfShiftsPerDepth, qwe, toMap, toMapMicro)
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
                print positionedBlocks

                -- V3 ->
                let gamma = barebonesGamma positionedBlocks

                putStrLn "barebones gamma:"
                print gamma

                let repositionedBlocks = processGammaShifts gamma positionedBlocks

                let gamma' = barebonesGamma repositionedBlocks

                putStrLn "barebones gamma':"
                print gamma'

                let origins = foldl (\accu ((_originX, originY), _gCId, _maxXOrigin) -> insert originY ((-1.0) * repositionShift) accu) empty gamma'
                putStrLn "origins:"
                print origins

                let destinations' = toMap repositionedBlocks

                let (gammaConnections, updatedOrigins, _destinations) =
                      foldl
                        ( \(accuGammaConnections, accuOrigins, accuDestinations) ((originX, originY), gCId, maxXOrigin) ->
                            let (connection, accuOrigins', destinations'') = buildGammaConnection gCId accuOrigins accuDestinations originX originY maxXOrigin
                             in (connection <> accuGammaConnections, accuOrigins', destinations'')
                        )
                        ([], origins, destinations')
                        gamma'

                putStrLn "updated origins:"
                print updatedOrigins

                let blockConnections3 = connectionsV2 repositionedBlocks
                let renderedConnections3 = renderConnections $ blockConnections3 <> gammaConnections

                -- rendering v3:
                renderSVG' ((outputPath input) <> "_v3") svgOptions ((render repositionedBlocks) <> renderedConnections3)

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
  [((Double, Double), ID, Double)] ->
  [[PositionedBlock]] ->
  [[PositionedBlock]]
processGammaShifts [] positionedBlocks = positionedBlocks
processGammaShifts (g@((xOrigin, yOrigin), gCId, maxXOrigin) : gs) positionedBlocks =
  let destinations = toMap positionedBlocks
      (repositionedBlocks, gs') = case Data.Map.lookup gCId destinations of
        Nothing -> (positionedBlocks, gs)
        Just (_dX, dY, _dMaxX, _dMinY, _gammaDShiftX, _gammaDShiftY) ->
          ( repositionX maxXOrigin (repositionTopY dY (repositionBottomY yOrigin positionedBlocks)),
            repositionOriginsTopY dY (repositionOrigins maxXOrigin yOrigin gs)
          )
   in processGammaShifts gs' repositionedBlocks
