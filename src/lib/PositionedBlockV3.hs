module PositionedBlockV3 where

import Constants
import Data.Map (Map, adjust, empty, insert, lookup)
import ID
import IntermediateBlock

-- maxX         - calculated on demand - only applies to blocks
-- maxXGamma    - calculated by the positioning code and unchanging - only applies to gamma connections
data PositionedBlockV3
  = PositionedStartTerminatorV3 Int Double Double Double Double Double Double
  | PositionedEndTerminatorV3 Int Double Double Double Double Double Double
  | PositionedActionV3 (Maybe ID) Int String Double Double Double Double Double Double
  | PositionedHeadlineV3 (Maybe ID) Int String Double Double Double Double Double Double
  | PositionedAddressV3 (Maybe ID) Int String Double Double Double Double Double Double
  | PositionedForkV3 (Maybe ID) Int String [PositionedBlockV3] [PositionedBlockV3] (Maybe ID) Double Double Double Double Double Double

instance Show PositionedBlockV3 where
  show (PositionedStartTerminatorV3 _pId x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = "StartTerminator"
  show (PositionedEndTerminatorV3 _pId x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = "PositionedEndTerminatorV3"
  show pB@(PositionedActionV3 i pId c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) =
    let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion, maxXGamma) = getPositionV3 pB
     in "Action [" <> show i <> "|" <> show pId <> "] x: " <> show x <> " y: " <> show y <> " maxX: " <> show maxX <> " minY: " <> show minY <> " maxXGamma: " <> show maxXGamma
  show (PositionedHeadlineV3 _i _pId c x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion maxXGamma) = "PositionedHeadlineV3"
  show (PositionedAddressV3 _i _pId c x y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion maxXGamma) = "PositionedAddressV3"
  show pB@(PositionedForkV3 i pId c l _r _gCId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) =
    let (x, y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion, maxXGamma) = getPositionV3 pB
     in "Fork [" <> show i <> "|" <> show pId <> "] x: " <> show x <> " y: " <> show y <> " maxX: " <> show maxX <> " minY: " <> show minY <> " maxXGamma: " <> show maxXGamma

getIDV3 :: PositionedBlockV3 -> Maybe ID
getIDV3 (PositionedStartTerminatorV3 _programmaticId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = Nothing
getIDV3 (PositionedEndTerminatorV3 _programmaticId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = Nothing
getIDV3 (PositionedActionV3 i _programmaticId _c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = i
getIDV3 (PositionedHeadlineV3 i _programmaticId _c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = i
getIDV3 (PositionedAddressV3 i _programmaticId _c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = i
getIDV3 (PositionedForkV3 i _programmaticId _c _l _r _gCId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = i

getProgrammaticIdV3 :: PositionedBlockV3 -> Int
getProgrammaticIdV3 (PositionedStartTerminatorV3 programmaticId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = programmaticId
getProgrammaticIdV3 (PositionedEndTerminatorV3 programmaticId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = programmaticId
getProgrammaticIdV3 (PositionedActionV3 _i programmaticId _c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = programmaticId
getProgrammaticIdV3 (PositionedHeadlineV3 _i programmaticId _c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = programmaticId
getProgrammaticIdV3 (PositionedAddressV3 _i programmaticId _c _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = programmaticId
getProgrammaticIdV3 (PositionedForkV3 _i programmaticId _c _l _r _gCId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) = programmaticId

getPositionV3 :: PositionedBlockV3 -> (Double, Double, Double, Double, Double, Double, Double, Double)
getPositionV3 (PositionedStartTerminatorV3 _pId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion, maxXGamma)
getPositionV3 (PositionedEndTerminatorV3 _pId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion, maxXGamma)
getPositionV3 (PositionedActionV3 _i _pId _c x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma) =
  let maxX = x + defaultBoundingBoxWidth + horizontalExpansion
   in ( x,
        y,
        maxX,
        y - defaultBoundingBoxHeight - verticalBottomExpansion - verticalTopExpansion,
        horizontalExpansion,
        verticalBottomExpansion,
        verticalTopExpansion,
        max maxXGamma maxX
      )
getPositionV3 (PositionedHeadlineV3 _i _pId _c x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion, maxXGamma)
getPositionV3 (PositionedAddressV3 _i _pId _c x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma) = (x, y, x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion, maxXGamma)
getPositionV3 (PositionedForkV3 _i _pId _c l r _gCId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma) =
  let (maxX, rMinY) = case r of
        [] -> (x + defaultBoundingBoxWidth, y - defaultBoundingBoxHeight)
        _ -> (getMaxX r, getMinY r)
      lMinY = case l of
        [] -> y - defaultBoundingBoxHeight
        _ -> getMinY l
      maxX' = maxX + horizontalExpansion
   in ( x,
        y,
        maxX',
        (min lMinY rMinY) - verticalBottomExpansion,
        horizontalExpansion,
        verticalBottomExpansion,
        verticalTopExpansion,
        max maxXGamma maxX'
      )

getMaxX :: [PositionedBlockV3] -> Double
getMaxX =
  foldr
    ( \b accu ->
        let (_x, _y, maxX, _minY, _horizontalExpansion, _verticalBottomExpansion, _verticalTopExpansion, _maxXGamma) = getPositionV3 b -- TODO: maxXGamma
         in max maxX accu
    )
    0.0

getMinY :: [PositionedBlockV3] -> Double
getMinY =
  foldr
    ( \b accu ->
        let (_x, _y, _maxX, minY, _horizontalExpansion, _verticalBottomExpansion, _verticalTopExpansion, _maxXGamma) = getPositionV3 b
         in min minY accu
    )
    0.0

position'' :: IntermediateBlock -> Double -> Double -> Double -> PositionedBlockV3
position'' (IntermediateStartTerminator programmaticId horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y maxXGamma = PositionedStartTerminatorV3 programmaticId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma
position'' (IntermediateEndTerminator programmaticId horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y maxXGamma = PositionedEndTerminatorV3 programmaticId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma
position'' (IntermediateAction userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y maxXGamma = PositionedActionV3 userId programmaticId content x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma
position'' (IntermediateHeadline userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y maxXGamma = PositionedHeadlineV3 userId programmaticId content x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma
position'' (IntermediateAddress userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y maxXGamma = PositionedAddressV3 userId programmaticId content x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma
position'' (IntermediateFork userId programmaticId content leftBranch rightBranch gammaConnectionDestinationUserId horizontalExpansion verticalBottomExpansion verticalTopExpansion) x y maxXGamma =
  let (positionedLeft, lMaxX, lMinY) = position' leftBranch x (y - defaultBoundingBoxHeight - verticalTopExpansion) x
      (positionedRight, rMaxX, rMinY) = position' rightBranch lMaxX (y - defaultBoundingBoxHeight - verticalTopExpansion) maxXGamma
   in PositionedForkV3 userId programmaticId content positionedLeft positionedRight gammaConnectionDestinationUserId x y horizontalExpansion verticalBottomExpansion verticalTopExpansion maxXGamma

position' :: [IntermediateBlock] -> Double -> Double -> Double -> ([PositionedBlockV3], Double, Double)
position' intermediateBlocks x y maxXGamma =
  let (_finalX, _finalY, finalPositionedBlocks, finalMaxX, finalMinY) =
        foldl
          ( \(accuX, accuY, accuPositionedBlocks, accuMaxX, accuMinY) iB ->
              let positionedBlock = position'' iB accuX accuY accuMaxX
                  (_x, _y, maxX, minY, horizontalExpansion, verticalBottomExpansion, verticalTopExpansion, maxXGamma') = getPositionV3 positionedBlock
               in ( accuX,
                    minY,
                    positionedBlock : accuPositionedBlocks,
                    maxXGamma' + repositionShift,
                    (min minY accuMinY)
                  )
          )
          (x, y, [], maxXGamma, y)
          intermediateBlocks
   in (finalPositionedBlocks, finalMaxX, finalMinY)

position :: [[IntermediateBlock]] -> Double -> Double -> [[PositionedBlockV3]]
position skewers x y =
  let (_finalMaxX, finalPositionedBlocks) =
        foldl
          ( \(accuMaxX, accuPositionedBlocks) skewer ->
              let (positionedSkewer, maxX, _minY) = position' skewer accuMaxX y accuMaxX
               in (maxX, (Prelude.reverse positionedSkewer) : accuPositionedBlocks)
          )
          (x, [])
          skewers
   in finalPositionedBlocks

positionedBlocksById'' :: (Map ID PositionedBlockV3, Map Int PositionedBlockV3) -> PositionedBlockV3 -> (Map ID PositionedBlockV3, Map Int PositionedBlockV3)
positionedBlocksById'' (userIdMap, programmaticIdMap) positionedBlock@(PositionedForkV3 userId programmaticId _content leftBranch rightBranch _gammaConnectionDestinationUserId _x _y _horizontalExpansion _verticalBottomExpansion _verticalTopExpansion _maxXGamma) =
  let (userIdMapL', programmaticIdMapL') = positionedBlocksById' (userIdMap, programmaticIdMap) leftBranch
      (userIdMapR', programmaticIdMapR') = positionedBlocksById' (userIdMapL', programmaticIdMapL') rightBranch
      userIdMap' = case userId of
        Nothing -> userIdMapR'
        (Just userId) -> Data.Map.insert userId positionedBlock userIdMapR'
      programmaticIdMap' = Data.Map.insert programmaticId positionedBlock programmaticIdMapR'
   in (userIdMap', programmaticIdMap')
positionedBlocksById'' (userIdMap, programmaticIdMap) positionedBlock =
  let userIdMap' = case getIDV3 positionedBlock of
        Nothing -> userIdMap
        (Just userId) -> Data.Map.insert userId positionedBlock userIdMap
      programmaticIdMap' = Data.Map.insert (getProgrammaticIdV3 positionedBlock) positionedBlock programmaticIdMap
   in (userIdMap', programmaticIdMap')

positionedBlocksById' :: (Map ID PositionedBlockV3, Map Int PositionedBlockV3) -> [PositionedBlockV3] -> (Map ID PositionedBlockV3, Map Int PositionedBlockV3)
positionedBlocksById' (userIdMap, programmaticIdMap) = foldl (\(accuUserIdMap, accuProgrammaticIdMap) positionedBlock -> positionedBlocksById'' (accuUserIdMap, accuProgrammaticIdMap) positionedBlock) (userIdMap, programmaticIdMap)

positionedBlocksById :: [[PositionedBlockV3]] -> (Map ID PositionedBlockV3, Map Int PositionedBlockV3)
positionedBlocksById = foldl (\(accuUserIdMap, accuProgrammaticIdMap) positionedSkewer -> positionedBlocksById' (accuUserIdMap, accuProgrammaticIdMap) positionedSkewer) (Data.Map.empty, Data.Map.empty)

originsAndDestinations :: (Map ID PositionedBlockV3, Map Int PositionedBlockV3) -> [(Int, ID)] -> ([((Double, Double, Double, Double), (Double, Double, Double, Double))], Map ID Double)
originsAndDestinations (userIdMap, programmaticIdMap) gammaConnections =
  foldl
    ( \(accuCoordinates, accuExpansions) (programmaticId, userId) ->
        let accuExpansions' = case Data.Map.lookup userId accuExpansions of
              Nothing -> Data.Map.insert userId 0.0 accuExpansions
              (Just expansion) -> Data.Map.adjust (\v -> v + repositionShift) userId accuExpansions
            origin = case Data.Map.lookup programmaticId programmaticIdMap of
              Nothing -> error $ "positioned block of programmatic id \"" <> show programmaticId <> "\" does not exist in the collection of blocks: " <> show programmaticIdMap
              (Just positionedBlock@(PositionedForkV3 _fUserId _fProgrammaticId _fContent _fLeftBranch fRightBranch _fGammaConnectionDestinationUserId _fX _fY _fHorizontalExpansion _fVerticalBottomExpansion _fVerticalTopExpansion _maxXGamma)) ->
                let (x, y, _maxX, minY, _horizontalExpansion, _verticalBottomExpansion, verticalTopExpansion, maxXGamma) = getPositionV3 positionedBlock
                    (x', y', maxX', minY') = case fRightBranch of
                      [] ->
                        -- 2026-10-13 PJ:
                        -- ==============
                        -- If the right hand side branch is empty, start
                        -- the gamma connection directly at the fork's question.
                        (x, y, maxXGamma, y - defaultBoundingBoxHeight * 0.5 - verticalTopExpansion)
                      _ -> (x, y, maxXGamma, minY)
                 in (x', y', maxX', minY')
              (Just positionedBlock) ->
                let (x, y, _maxX, minY, _horizontalExpansion, _verticalBottomExpansion, _verticalTopExpansion, maxXGamma) = getPositionV3 positionedBlock
                 in (x, y, maxXGamma, minY)
            destination = case Data.Map.lookup userId userIdMap of
              Nothing -> error $ "positioned block of user id \"" <> show userId <> "\" does not exist in the collection of blocks: " <> show userIdMap
              (Just positionedBlock) ->
                let (x, y, _maxX, minY, _horizontalExpansion, _verticalBottomExpansion, verticalTopExpansion, maxXGamma) = getPositionV3 positionedBlock
                    expansion' = case Data.Map.lookup userId accuExpansions' of
                      Nothing -> error $ "expansion associated with user id \"" <> show userId <> "\" does not exist in the collection of expansions: " <> show accuExpansions'
                      (Just expansion) -> expansion
                 in (x, y - expansion', maxXGamma, minY)
         in ((origin, destination) : accuCoordinates, accuExpansions')
    )
    ([], Data.Map.empty)
    gammaConnections
