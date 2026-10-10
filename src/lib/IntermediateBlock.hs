module IntermediateBlock where

import Blocks
import ID

data IntermediateBlock
  = IntermediateStartTerminator Int Double Double Double
  | IntermediateEndTerminator Int Double Double Double
  | IntermediateAction (Maybe ID) Int String Double Double Double
  | IntermediateHeadline (Maybe ID) Int String Double Double Double
  | IntermediateAddress (Maybe ID) Int String Double Double Double
  | IntermediateFork (Maybe ID) Int String [IntermediateBlock] [IntermediateBlock] (Maybe ID) Double Double Double

genericShow :: (Maybe ID) -> Int -> String -> Double -> Double -> Double -> String
genericShow
  Nothing
  programmaticId
  content
  horizontalExpansion
  verticalBottomExpansion
  verticalTopExpansion =
    "programmatic id: "
      <> show programmaticId
      <> ", content: "
      <> content
      <> ", horizontal expansion: "
      <> show horizontalExpansion
      <> ", vertical bottom expansion: "
      <> show verticalBottomExpansion
      <> ", vertical top expansion: "
      <> show verticalTopExpansion
genericShow
  (Just userId)
  programmaticId
  content
  horizontalExpansion
  verticalBottomExpansion
  verticalTopExpansion =
    "user id: "
      <> show userId
      <> ", "
      <> genericShow Nothing programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion

instance Show IntermediateBlock where
  show (IntermediateStartTerminator programmaticId horizontalExpansion verticalBottomExpansion verticalTopExpansion) = "IntermediateStartTerminator\t| " <> genericShow Nothing programmaticId "" horizontalExpansion verticalBottomExpansion verticalTopExpansion
  show (IntermediateEndTerminator programmaticId horizontalExpansion verticalBottomExpansion verticalTopExpansion) = "IntermediateEndTerminator | " <> genericShow Nothing programmaticId "" horizontalExpansion verticalBottomExpansion verticalTopExpansion
  show (IntermediateAction userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) = "IntermediateAction\t| " <> genericShow userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion
  show (IntermediateHeadline userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) = "IntermediateHeadline\t| " <> genericShow userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion
  show (IntermediateAddress userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion) = "IntermediateAddress\t| " <> genericShow userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion
  show (IntermediateFork userId programmaticId content leftBranch rightBranch gammaConnectionDestinationUserId horizontalExpansion verticalBottomExpansion verticalTopExpansion) =
    "IntermediateFork\t| "
      <> genericShow userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion
      <> "\nleft:\n"
      <> showBlocks' leftBranch
      <> "right:\n"
      <> showBlocks' rightBranch
      <> "gamma connection destination: "
      <> show gammaConnectionDestinationUserId

showBlocks' :: [IntermediateBlock] -> String
showBlocks' = foldl (\accu intermediateBlock -> accu <> show intermediateBlock <> "\n") ""

showBlocks :: [[IntermediateBlock]] -> String
showBlocks = foldl (\accu intermediateBlocks -> accu <> showBlocks' intermediateBlocks) ""

fromBlock :: Int -> [(Int, ID)] -> Block -> (IntermediateBlock, Int, [(Int, ID)])
fromBlock programmaticId gammaConnections StartTerminator = (IntermediateStartTerminator programmaticId 0.0 0.0 0.0, programmaticId + 1, gammaConnections)
fromBlock programmaticId gammaConnections EndTerminator = (IntermediateEndTerminator programmaticId 0.0 0.0 0.0, programmaticId + 1, gammaConnections)
fromBlock programmaticId gammaConnections (Action userId content) = (IntermediateAction userId programmaticId content 0.0 0.0 0.0, programmaticId + 1, gammaConnections)
fromBlock programmaticId gammaConnections (Headline userId content) = (IntermediateHeadline userId programmaticId content 0.0 0.0 0.0, programmaticId + 1, gammaConnections)
fromBlock programmaticId gammaConnections (Address userId content) = (IntermediateAddress userId programmaticId content 0.0 0.0 0.0, programmaticId + 1, gammaConnections)
fromBlock programmaticId gammaConnections (Fork userId content leftBranch rightBranch gammaConnectionDestinationUserId) =
  let (leftBranch', programmaticIdLeft, gammaConnectionsLeft) = fromBlocks' (programmaticId + 1) gammaConnections leftBranch
      (rightBranch', programmaticIdRight, gammaConnectionsRight) = fromBlocks' programmaticIdLeft gammaConnectionsLeft rightBranch
      gammaConnections' = case gammaConnectionDestinationUserId of
        Nothing -> gammaConnectionsRight
        (Just gammaConnectionDestinationUserId') -> (programmaticIdLeft, gammaConnectionDestinationUserId') : gammaConnectionsRight
   in ( ( IntermediateFork
            userId
            programmaticId
            content
            leftBranch'
            rightBranch'
            gammaConnectionDestinationUserId
            0.0
            0.0
            0.0
        ),
        programmaticIdRight,
        gammaConnections'
      )

fromBlocks' :: Int -> [(Int, ID)] -> [Block] -> ([IntermediateBlock], Int, [(Int, ID)])
fromBlocks' startingProgrammaticId gammaConnections =
  foldl
    ( \(accuBlocks, accuProgrammaticId, accuGammaConnections) block ->
        let (intermediateBlock, programmaticId, gammaConnections') = fromBlock accuProgrammaticId accuGammaConnections block
         in (intermediateBlock : accuBlocks, programmaticId, gammaConnections')
    )
    ([], startingProgrammaticId, gammaConnections)

fromBlocks :: [[Block]] -> ([[IntermediateBlock]], [(Int, ID)])
fromBlocks skewers =
  let (intermediateBlocks, _programmaticId, gammaConnections) =
        foldl
          ( \(accuSkewers, accuProgrammaticId, accuGammaConnections) blocks ->
              let (skewer, programmaticId, gammaConnections') = fromBlocks' accuProgrammaticId accuGammaConnections blocks
               in (skewer : accuSkewers, programmaticId, gammaConnections')
          )
          ([], 0, [])
          skewers
   in (intermediateBlocks, gammaConnections)

expandSource'' :: IntermediateBlock -> Int -> ID -> Double -> Double -> Double -> IntermediateBlock
expandSource''
  (IntermediateFork userId programmaticId content leftBranch rightBranch gammaConnectionDestinationUserId horizontalExpansion verticalBottomExpansion verticalTopExpansion)
  programmaticId'
  userId''
  horizontalExpansion'
  verticalBottomExpansion'
  verticalTopExpansion' =
    let leftBranch' = expandSource' leftBranch programmaticId' userId'' horizontalExpansion' verticalBottomExpansion' verticalTopExpansion'
        rightBranch' = expandSource' rightBranch programmaticId' userId'' horizontalExpansion' verticalBottomExpansion' verticalTopExpansion'
        horizontalExpansion'' = if programmaticId == programmaticId' then horizontalExpansion + horizontalExpansion' else horizontalExpansion
        verticalBottomExpansion'' = if programmaticId == programmaticId' then verticalBottomExpansion + verticalBottomExpansion' else verticalBottomExpansion
        verticalTopExpansion'' = case userId of
          Nothing -> verticalTopExpansion
          (Just userId') -> if userId' == userId'' then verticalTopExpansion + verticalTopExpansion' else verticalTopExpansion
     in IntermediateFork
          userId
          programmaticId
          content
          leftBranch'
          rightBranch'
          gammaConnectionDestinationUserId
          horizontalExpansion''
          verticalBottomExpansion''
          verticalTopExpansion''
expandSource''
  iB@(IntermediateAction userId programmaticId content horizontalExpansion verticalBottomExpansion verticalTopExpansion)
  programmaticId'
  userId''
  horizontalExpansion''
  verticalBottomExpansion''
  verticalTopExpansion'' =
    let horizontalExpansion' = if programmaticId == programmaticId' then horizontalExpansion + horizontalExpansion'' else horizontalExpansion
        verticalBottomExpansion' = if programmaticId == programmaticId' then verticalBottomExpansion + verticalBottomExpansion'' else verticalBottomExpansion
        verticalTopExpansion' =
          case userId of
            Nothing -> verticalTopExpansion
            (Just userId') -> if userId' == userId'' then verticalTopExpansion + verticalTopExpansion'' else verticalTopExpansion
     in IntermediateAction userId programmaticId content horizontalExpansion' verticalBottomExpansion' verticalTopExpansion'
-- TODO: replace with real constructors
expandSource'' intermediateBlock _programmaticId' _userId' _horizontalExpansion' _verticalBottomExpansion' _verticalTopExpansion' = intermediateBlock

expandSource' ::
  [IntermediateBlock] ->
  Int ->
  ID ->
  Double ->
  Double ->
  Double ->
  [IntermediateBlock]
expandSource'
  intermediateBlocks
  programmaticId'
  userId'
  horizontalExpansion'
  verticalBottomExpansion'
  verticalTopExpansion' =
    foldl (\accu iB -> (expandSource'' iB programmaticId' userId' horizontalExpansion' verticalBottomExpansion' verticalTopExpansion') : accu) [] intermediateBlocks

expandSource ::
  [[IntermediateBlock]] ->
  Int ->
  ID ->
  Double ->
  Double ->
  Double ->
  [[IntermediateBlock]]
expandSource
  intermediateSkewers
  programmaticId'
  userId'
  horizontalExpansion'
  verticalBottomExpansion'
  verticalTopExpansion' =
    foldl (\accu iS -> (expandSource' iS programmaticId' userId' horizontalExpansion' verticalBottomExpansion' verticalTopExpansion') : accu) [] intermediateSkewers

gammaExpansions :: [[IntermediateBlock]] -> [(Int, ID)] -> [[IntermediateBlock]]
gammaExpansions intermediateBlocks =
  foldl (\accu (gammaConnectionOriginProgrammaticId, gammaConnectionDestinationUserId) -> expandSource accu gammaConnectionOriginProgrammaticId gammaConnectionDestinationUserId 0.5 0.5 0.5) intermediateBlocks
