module Main where

import Blocks (renderDiagram, reverse)
import Constants (svgOptions)
import Data.Map (lookup)
import Diagrams.Backend.SVG (renderSVG')
import ID
import InputArguments (inputPath, outputPath, parseInput)
import Layout (barebonesGamma, buildGammaConnection, connectionsV2, position, repositionBasedOnGamma, repositionBasedOnGammaX, repositionOriginsBasedOnGamma)
import Lexer (lexAll, runAlex)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Parser (ParseResult (..), diagram)
import PositionedBlock
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
                print positionedBlocks

                -- V3 ->
                let gamma = barebonesGamma positionedBlocks

                putStrLn "barebones gamma:"
                print gamma

                -- let (gammaConnections, repositionedBlocks) =
                --       foldr
                --         ( \((xOrigin, yOrigin), gCId, maxXOrigin) (accuGammaConnections, accuPositionedBlocks) ->
                --             let destinations = toMap accuPositionedBlocks
                --                 repositionedBlocks = case Data.Map.lookup gCId destinations of
                --                   Nothing -> accuPositionedBlocks
                --                   Just (_x, y, maxX, _minY, _gammaShiftX, _gammaShiftY) -> repositionBasedOnGammaX (repositionBasedOnGamma accuPositionedBlocks [(y, 1)]) [(maxX, 1)]
                --                 gammaConnection = fst $ buildGammaConnection gCId destinations xOrigin yOrigin maxXOrigin
                --              in (gammaConnection <> accuGammaConnections, repositionedBlocks)
                --         )
                --         ([], positionedBlocks)
                --         gamma

                let (_iDs, repositionedBlocks) = processGammaShifts gamma [] positionedBlocks

                let gamma' = barebonesGamma repositionedBlocks

                putStrLn "barebones gamma':"
                print gamma'

                let destinations' = toMap repositionedBlocks

                let gammaConnections =
                      foldr
                        ( \((originX, originY), gCId, maxXOrigin) accu ->
                            (fst $ buildGammaConnection gCId destinations' originX originY maxXOrigin) <> accu
                        )
                        []
                        gamma'

                let blockConnections3 = connectionsV2 repositionedBlocks
                let renderedConnections3 = renderConnections $ blockConnections3 <> gammaConnections
                -- let renderedConnections3 = renderConnections $ blockConnections3
                -- <- V3

                let destinationsMicro = toMapMicro positionedBlocks
                -- TODO:
                -- Probably a good idea to preserve those steps for debugging just in case.
                let gammaConnections = extractGammaConnections destinationsMicro positionedBlocks
                -- print gammaConnections

                let defInput = qwe gammaConnections
                -- print defInput

                let maxNumberOfShiftsPerDepth = getMaxNumberOfShiftsPerDepth defInput
                -- print maxNumberOfShiftsPerDepth

                let repositionedBlocks' = repositionBasedOnGamma positionedBlocks maxNumberOfShiftsPerDepth
                -- putStrLn "positionedBlocks:"
                -- print positionedBlocks
                -- putStrLn "repositionedBlocks':"
                -- print repositionedBlocks'

                let destinations = toMap repositionedBlocks'

                let blockConnections2 = connectionsV2 repositionedBlocks'
                let renderedConnections2 = renderConnections blockConnections2

                let gamma2 = barebonesGamma repositionedBlocks'
                -- putStrLn "barebones gamma:"
                -- print gamma

                let (gammaConnections', updatedDestinations) =
                      foldl
                        ( \(accuConnections, accuDestinations) singleGamma@((originX, originY), gCId, originMaxX) ->
                            let (newConnections, accuDestinations') = buildGammaConnection gCId accuDestinations originX originY originMaxX
                             in (accuConnections <> newConnections, accuDestinations')
                        )
                        ([], destinations)
                        gamma2

                -- print destinations
                -- print updatedDestinations

                -- rendering v3:
                -- renderSVG' ((outputPath input) <> "_v3") svgOptions ((render repositionedBlocks) <> renderedConnections3)

                -- rendering v2:
                -- renderSVG' ((outputPath input) <> "_new") svgOptions ((render repositionedBlocks) <> renderConnections (blockConnections2 <> gammaConnections'))

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

processGammaShifts ::
  [((Double, Double), ID, Double)] ->
  [((Double, Double), ID, Double)] ->
  [[PositionedBlock]] ->
  ([((Double, Double), ID, Double)], [[PositionedBlock]])
processGammaShifts [] processedGammaConnections positionedBlocks = (processedGammaConnections, positionedBlocks)
processGammaShifts (g@((xOrigin, yOrigin), gCId, maxXOrigin) : gs) processedGammaConnections positionedBlocks =
  let destinations = toMap positionedBlocks
      (repositionedBlocks, gs') = case Data.Map.lookup gCId destinations of
        Nothing -> (positionedBlocks, gs)
        Just (_x, y, maxX, _minY, _gammaShiftX, _gammaShiftY) ->
          ( repositionBasedOnGammaX (repositionBasedOnGamma positionedBlocks [(y, 1)]) [(maxXOrigin, 1)],
            repositionOriginsBasedOnGamma maxXOrigin y gs
          )
   in processGammaShifts gs' (g : processedGammaConnections) repositionedBlocks
