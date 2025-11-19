module SkewerBlock where

import Constants
import Content
import Data.Map (Map, empty, insert, lookup)
import Data.String.Utils (lstrip)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position, r2, rotateBy, translate, triangle, (#))
import HelperDiagrams
import ID

loopbackConflict' :: [Point V2 Double] -> Double -> Double -> Double -> Bool
loopbackConflict' [] _ _ _ = False
loopbackConflict'
  ( p0
      : (P (V2 x1 y1))
      : (P (V2 x2 y2))
      : p3
      : []
    )
  x
  y1'
  y2' =
    if (y1' < y1 && y2' < y1 && y1' < y2 && y2' < y2)
      then False
      else
        if y2' >= y2
          then
            if x == x2
              then True
              else False
          else False -- 2025-09-03 PJ: illegal state
loopbackConflict' _ _ _ _ = False

loopbackConflict :: [[Point V2 Double]] -> Double -> Double -> Double -> Double
loopbackConflict [] x _ _ = x
loopbackConflict loopbackConnections x y1' y2' =
  foldr
    (\loopbackConnection accu -> if loopbackConflict' loopbackConnection accu y1' y2' then accu + 0.1 else accu)
    x
    loopbackConnections

renderAdditionalConnection ::
  Point V2 Double ->
  Maybe ID ->
  Map ID (Point V2 Double) ->
  Double ->
  [[Point V2 Double]] ->
  (Diagram B, [[Point V2 Double]])
renderAdditionalConnection sourceOrigin@(P (V2 x1 y1)) Nothing _ _ existingLoopbacks = (mempty, existingLoopbacks)
renderAdditionalConnection sourceOrigin@(P (V2 x1 y1)) (Just destinationId) mapOfOrigins widthToBypass existingLoopbacks =
  case Data.Map.lookup destinationId mapOfOrigins of
    (Just _destinationOrigin@(P (V2 x2 y2))) ->
      if x1 > x2 && y1 < y2
        then
          let midpointX = x1 + widthToBypass
              y1' = y1
              y2' = y2 - 0.1
              updatedMidpointX = loopbackConflict existingLoopbacks midpointX y1' y2'
              points =
                [ sourceOrigin,
                  p2 (updatedMidpointX, y1'),
                  p2 (updatedMidpointX, y2'),
                  p2 (x2 + defaultBoundingBoxWidth * 0.5 + 0.087, y2')
                ]
           in ( renderConnection points
                  <> position
                    [ ( p2 (x2 + defaultBoundingBoxWidth * 0.5 + (0.087 / 2.0) + 0.02, y2 - 0.1),
                        rotateBy (1 / 4) $ triangle 0.1 # drakonStyle
                      )
                    ],
                points : existingLoopbacks
              )
        else
          ( if (x1 < x2 && y1 > y2) || (x1 > x2 && y1 > y2)
              then
                let points =
                      [ sourceOrigin,
                        p2 (x2 + defaultBoundingBoxWidth - 0.1, y1),
                        p2 (x2 + defaultBoundingBoxWidth - 0.1, y2 + 0.1),
                        p2 (x2 + defaultBoundingBoxWidth * 0.5, y2 + 0.1)
                      ]
                 in (renderConnection points, points : existingLoopbacks)
              else (renderConnection [sourceOrigin, _destinationOrigin], [sourceOrigin, _destinationOrigin] : existingLoopbacks)
          )
    -- 0.087:   from Pythegorean theorem
    -- 0.02:  from line width?
    -- 2025-08-23 PJ:
    -- --------------
    -- TODO: error here
    _ -> mempty

renderSkewerBlocks ::
  Double ->
  Double ->
  [SkewerBlock] ->
  Map ID (Point V2 Double) ->
  [[Point V2 Double]] ->
  (Diagram B, Double, [[Point V2 Double]])
renderSkewerBlocks connectionX lastY skewerBlocks mapOfOrigins existingLoopbacks =
  foldl
    ( \accu singleBlock ->
        let (diagram, preY1, accuLoopbackConnections) = accu
            preY2 = preY1 - defaultBoundingBoxHeight * 0.25
            postY1 = preY2 - defaultBoundingBoxHeight * 0.5
            postY2 = preY1 - defaultBoundingBoxHeight
            (P (V2 x y)) = getOrigin singleBlock
            shiftedSingleBlock = if x == connectionX then singleBlock else (changeOrigin singleBlock (P (V2 (connectionX - defaultBoundingBoxWidth * 0.5) y)))
            (renderedSingleBlock, loopbackConnections) = render shiftedSingleBlock mapOfOrigins accuLoopbackConnections
         in ( renderConnection [p2 (connectionX, preY1), p2 (connectionX, preY2)]
                <> diagram
                <> renderConnection [p2 (connectionX, postY1), p2 (connectionX, postY2)]
                <> renderedSingleBlock,
              preY1 - heightInUnits shiftedSingleBlock * defaultBoundingBoxHeight,
              loopbackConnections
            )
    )
    (mempty, lastY, existingLoopbacks)
    skewerBlocks

renderLeft ::
  ConnectedSkewerBlocks ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [[Point V2 Double]] ->
  (Diagram B, Double, [[Point V2 Double]])
renderLeft (ConnectedSkewerBlocks skewerBlocks id) (P (V2 x y)) mapOfOrigins existingLoopbacks =
  let skewerWidth = (widthInUnits' skewerBlocks) * defaultBoundingBoxWidth
   in if null skewerBlocks
        then
          let connectionX = x - defaultBoundingBoxWidth * (1 - widthRatio) / 2.0
              (additionalConnection, loopbackConnections) =
                renderAdditionalConnection
                  (p2 (connectionX, y + defaultBoundingBoxHeight * 0.5))
                  id
                  mapOfOrigins
                  skewerWidth
                  existingLoopbacks
           in (additionalConnection, y, loopbackConnections)
        else
          ( let connectionX = x + defaultBoundingBoxWidth * 0.5
                (renderedBlocks, lastY, innerLoopbackConnections) = renderSkewerBlocks connectionX y skewerBlocks mapOfOrigins existingLoopbacks
                (additionalConnection, loopbackConnections) =
                  renderAdditionalConnection
                    (p2 (connectionX, lastY))
                    id
                    mapOfOrigins
                    skewerWidth
                    innerLoopbackConnections
             in (renderedBlocks <> additionalConnection, lastY, loopbackConnections)
          )

renderRight ::
  ConnectedSkewerBlocks ->
  Point V2 Double ->
  Point V2 Double ->
  Map ID (Point V2 Double) ->
  [[Point V2 Double]] ->
  (Diagram B, Double, [[Point V2 Double]])
renderRight
  (ConnectedSkewerBlocks skewerBlocks id)
  leftOrigin@(P (V2 lx ly))
  _rightOrigin@(P (V2 rx ry))
  mapOfOrigins
  existingLoopbacks =
    if null skewerBlocks
      then
        let connectionX = lx + defaultBoundingBoxWidth * 0.5
            (additionalConnection, loopbackConnections) =
              renderAdditionalConnection
                (p2 (connectionX, ry + defaultBoundingBoxHeight * 0.5))
                id
                mapOfOrigins
                5.0 -- TODO
                existingLoopbacks
         in (additionalConnection, ry, loopbackConnections)
      else
        ( let connectionX = rx + defaultBoundingBoxWidth * 0.5
              (renderedBlocks, lastY, innerLoopbackConnections) = renderSkewerBlocks rx ry skewerBlocks mapOfOrigins existingLoopbacks
              (additionalConnection, loopbackConnections) =
                renderAdditionalConnection
                  (p2 (connectionX, lastY))
                  id
                  mapOfOrigins
                  ((widthInUnits' skewerBlocks) * defaultBoundingBoxWidth)
                  innerLoopbackConnections
           in (renderedBlocks <> additionalConnection, lastY, loopbackConnections)
        )

renderIcons' :: [SkewerBlock] -> Map ID (Point V2 Double) -> (Diagram B, Double, [[Point V2 Double]])
renderIcons' skewerBlocks mapOfOrigins =
  foldl
    ( \accu singleBlock ->
        let (currentDiagram, lastBlocksDepth, accuLoopbackConnections) = accu
            (P (V2 x preY1)) = getOrigin singleBlock
            connectionX = x + defaultBoundingBoxWidth * 0.5
            preY2 = preY1 - defaultBoundingBoxHeight * blockHeightOffsetInUnits singleBlock
            postY1 = preY2 - defaultBoundingBoxHeight * blockHeightInUnits singleBlock
            postY2 = preY1 - defaultBoundingBoxHeight
            (renderedSingleBlock, updatedLoopbackConnections) = render singleBlock mapOfOrigins accuLoopbackConnections
         in ( currentDiagram
                <> ( case singleBlock of
                       Address {} -> renderConnection [p2 (connectionX, lastBlocksDepth), p2 (connectionX, preY1)]
                       _ -> mempty
                   )
                <> renderConnection [p2 (connectionX, preY1), p2 (connectionX, preY2)]
                <> renderedSingleBlock
                <> renderConnection [p2 (connectionX, postY1), p2 (connectionX, postY2)],
              postY2,
              updatedLoopbackConnections
            )
    )
    (mempty, 0.0, [])
    skewerBlocks

renderIcons :: [SkewerBlock] -> Map ID (Point V2 Double) -> Double -> Diagram B
renderIcons skewerBlocks mapOfOrigins addressDepth =
  let (P (V2 firstBlockX _)) = getOrigin $ head skewerBlocks
      (renderedIcons, skewerDepth, allLoopbackConnections) = renderIcons' skewerBlocks mapOfOrigins
      connectionForMissingAddress =
        -- 2025-08-24 PJ:
        -- --------------
        -- Only needed for debugging.
        if skewerDepth < addressDepth
          then mempty
          else
            -- this is incorrect for input like:
            -- https://github.com/PiotrJustyna/wyvern/issues/24#issuecomment-3214758752
            -- move the second x by 0.1 and you'll see the problem
            -- for the linked input
            renderConnection
              [ p2 (firstBlockX + defaultBoundingBoxWidth * 0.5, skewerDepth),
                p2 (firstBlockX + defaultBoundingBoxWidth * 0.5, addressDepth - defaultBoundingBoxHeight)
              ]
   in renderedIcons <> connectionForMissingAddress

position' :: [SkewerBlock] -> Point V2 Double -> Double -> [SkewerBlock]
position' skewerBlocks (P (V2 x y)) addressDepth =
  fst $
    foldl
      ( \accu singleBlock ->
          case singleBlock of
            Address {} ->
              let positionedSkewerBlocks = fst accu
               in ( positionedSkewerBlocks <> [changeOrigin singleBlock (P (V2 x addressDepth))],
                    addressDepth - heightInUnits singleBlock * defaultBoundingBoxHeight
                  )
            _ ->
              let positionedSkewerBlocks = fst accu
               in ( positionedSkewerBlocks <> [changeOrigin singleBlock (P (V2 x (snd accu)))],
                    snd accu - heightInUnits singleBlock * defaultBoundingBoxHeight
                  )
      )
      ([], y)
      skewerBlocks

widthInUnits' :: [SkewerBlock] -> Double
widthInUnits' x =
  case x of
    [] -> 1.0
    skewerBlocks -> maximum $ map widthInUnits skewerBlocks

heightInUnits' :: [SkewerBlock] -> Double
heightInUnits' skewerBlocks = sum $ map heightInUnits skewerBlocks

reverse'' :: [[SkewerBlock]] -> [[SkewerBlock]]
reverse'' = foldl (\accu x -> reverse' x : accu) []

reverse' :: [SkewerBlock] -> [SkewerBlock]
reverse' =
  foldl
    ( \accu x ->
        case x of
          Fork id origin content (ConnectedSkewerBlocks l lId) (ConnectedSkewerBlocks r rId) ->
            Fork id origin content (ConnectedSkewerBlocks (reverse' l) lId) (ConnectedSkewerBlocks (reverse' r) rId)
              : accu
          _ -> x : accu
    )
    []

toMap :: [SkewerBlock] -> Map ID (Point V2 Double)
toMap = foldl (flip insertToMap) empty

data ConnectedSkewerBlocks
  = ConnectedSkewerBlocks [SkewerBlock] (Maybe ID)
  deriving (Show)

data SkewerBlock
  = Action ID (Point V2 Double) Content
  | Headline ID (Point V2 Double) Content
  | Address ID (Point V2 Double) Content
  | Fork ID (Point V2 Double) Content ConnectedSkewerBlocks ConnectedSkewerBlocks

toId :: String -> ID
toId x = ID (head $ words x)

toContent :: String -> Content
toContent text =
  let id = head $ words text
      idFreeContent = lstrip (drop (length id) text)
      idFreeContent' = drop 1 idFreeContent
      idFreeContent'' = take (length idFreeContent' - 1) idFreeContent'
   in Content idFreeContent''

toHeadline :: String -> SkewerBlock
toHeadline x = Headline (toId x) (p2 (-1.0, -1.0)) (toContent x)

toAddress :: String -> SkewerBlock
toAddress x = Address (toId x) (p2 (-1.0, -1.0)) (Content (head $ words x))

toAction :: String -> SkewerBlock
toAction x = Action (toId x) (p2 (-1.0, -1.0)) (toContent x)

toFork :: String -> [SkewerBlock] -> Maybe ID -> [SkewerBlock] -> Maybe ID -> SkewerBlock
toFork x l lId r rId =
  Fork (toId x) (p2 (-1.0, -1.0)) (toContent x) (ConnectedSkewerBlocks l lId) (ConnectedSkewerBlocks r rId)

getOrigin :: SkewerBlock -> Point V2 Double
getOrigin (Action _ origin _) = origin
getOrigin (Headline _ origin _) = origin
getOrigin (Address _ origin _) = origin
getOrigin (Fork _ origin _ _ _) = origin

insertToMap :: SkewerBlock -> Map ID (Point V2 Double) -> Map ID (Point V2 Double)
insertToMap skewerBlock@(Action actionId _ _) startingMap = insert actionId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Headline headlineId _ _) startingMap = insert headlineId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Address addressId _ _) startingMap = insert addressId (getOrigin skewerBlock) startingMap
insertToMap skewerBlock@(Fork forkId _ _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) startingMap =
  let leftMap = toMap l
      rightMap = toMap r
   in insert forkId (getOrigin skewerBlock) (startingMap <> leftMap <> rightMap)

-- 2025-09-04 PJ:
-- --------------
-- removed the hardcoded value -18.0
-- l and r origin calculated twice:

-- * here

-- * in the render function

changeOrigin :: SkewerBlock -> Point V2 Double -> SkewerBlock
changeOrigin (Action actionId _ content) newOrigin = Action actionId newOrigin content
changeOrigin (Headline headlineId _ content) newOrigin = Headline headlineId newOrigin content
changeOrigin (Address addressId _ content) newOrigin = Address addressId newOrigin content
changeOrigin (Fork forkId _ content (ConnectedSkewerBlocks l leftId) (ConnectedSkewerBlocks r rightId)) newOrigin@(P (V2 x y)) =
  let lOrigin = P (V2 x (y - defaultBoundingBoxHeight))
      rOrigin = P (V2 (x + widthInUnits' l * defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight))
      newL = ConnectedSkewerBlocks (position' l lOrigin (-18.0)) leftId
      newR = ConnectedSkewerBlocks (position' r rOrigin (-18.0)) rightId
   in Fork forkId newOrigin content newL newR

instance Show SkewerBlock where
  show (Action (ID actionId) origin (Content content)) =
    "[ID: " <> actionId <> " | Origin: " <> show origin <> "] " <> content
  show (Headline (ID headlineId) origin (Content content)) =
    "[ID: " <> headlineId <> " | Origin: " <> show origin <> "] " <> content
  show (Address (ID addressId) origin (Content content)) =
    "[ID: " <> addressId <> " | Origin: " <> show origin <> "] " <> content
  show (Fork (ID forkId) origin (Content content) _ _) =
    "[ID: " <> forkId <> " | Origin: " <> show origin <> "] " <> content

renderQuestion :: ID -> Point V2 Double -> Content -> Map ID (Point V2 Double) -> Diagram B
renderQuestion questionId origin (Content content) _mapOfOrigins =
  position
    [ ( origin,
        renderText
          ( ( if troubleshootingMode
                then "[" <> show questionId <> " | " <> show origin <> "] "
                else ""
            )
              <> content
          )
          (0.0 + defaultBoundingBoxWidth * 0.5)
          (0.0 - defaultBoundingBoxHeight * 0.5)
          <> hex' (defaultBoundingBoxWidth * widthRatio) (defaultBoundingBoxHeight * 0.5)
            # translate
              (r2 (0.1 + defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, defaultBoundingBoxHeight * (-0.25)))
          <> if troubleshootingMode
            then boundingBox defaultBoundingBoxWidth defaultBoundingBoxHeight
            else mempty
      )
    ]

render ::
  SkewerBlock ->
  Map ID (Point V2 Double) ->
  [[Point V2 Double]] ->
  (Diagram B, [[Point V2 Double]])
render action@(Action actionId origin (Content actionContent)) _mapOfOrigins existingLoopbacks =
  let iconHeight = heightInUnits action * defaultBoundingBoxHeight * 0.5
   in ( position
          [ ( origin,
              renderText
                ( ( if troubleshootingMode
                      then "[" <> show actionId <> " | " <> show origin <> "] "
                      else ""
                  )
                    <> actionContent
                )
                (0.0 + widthInUnits action * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits action * defaultBoundingBoxHeight * 0.5)
                <> rect' (widthInUnits action * defaultBoundingBoxWidth * widthRatio) iconHeight
                  # translate (r2 (defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                  then
                    boundingBox
                      (widthInUnits action * defaultBoundingBoxWidth)
                      (heightInUnits action * defaultBoundingBoxHeight)
                  else mempty
            )
          ],
        existingLoopbacks
      )
render headline@(Headline headlineId origin (Content headlineContent)) _mapOfOrigins existingLoopbacks =
  let iconHeight = heightInUnits headline * defaultBoundingBoxHeight * 0.5
   in ( position
          [ ( origin,
              renderText
                ( ( if troubleshootingMode
                      then "[" <> show headlineId <> " | " <> show origin <> "] "
                      else ""
                  )
                    <> headlineContent
                )
                (0.0 + widthInUnits headline * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits headline * defaultBoundingBoxHeight * 0.5)
                <> headlineShape (widthInUnits headline * defaultBoundingBoxWidth * widthRatio) iconHeight
                  # translate (r2 (defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                  then
                    boundingBox
                      (widthInUnits headline * defaultBoundingBoxWidth)
                      (heightInUnits headline * defaultBoundingBoxHeight)
                  else mempty
            )
          ],
        existingLoopbacks
      )
render address@(Address addressId origin (Content addressContent)) _mapOfOrigins existingLoopbacks =
  let iconHeight = heightInUnits address * defaultBoundingBoxHeight * 0.5
   in ( position
          [ ( origin,
              renderText
                ( ( if troubleshootingMode
                      then "[" <> show addressId <> " | " <> show origin <> "] "
                      else ""
                  )
                    <> addressContent
                )
                (0.0 + widthInUnits address * defaultBoundingBoxWidth * 0.5)
                (0.0 - heightInUnits address * defaultBoundingBoxHeight * 0.5)
                <> addressShape (widthInUnits address * defaultBoundingBoxWidth * widthRatio) iconHeight
                  # translate (r2 (defaultBoundingBoxWidth * (1 - widthRatio) / 2.0, iconHeight * (-0.5)))
                <> if troubleshootingMode
                  then
                    boundingBox
                      (widthInUnits address * defaultBoundingBoxWidth)
                      (heightInUnits address * defaultBoundingBoxHeight)
                  else mempty
            )
          ],
        existingLoopbacks
      )
render
  fork@(Fork forkId origin@(P (V2 x y)) content leftBranch@(ConnectedSkewerBlocks l lDetourId) rightBranch@(ConnectedSkewerBlocks r rDetourId))
  _mapOfOrigins
  existingLoopbacks =
    let lOrigin@(P (V2 _ lY)) = P (V2 x (y - defaultBoundingBoxHeight))
        (renderedLBranch, _, lLoopbackConnections) = renderLeft leftBranch lOrigin _mapOfOrigins existingLoopbacks
        rOrigin@(P (V2 rX rY)) = P (V2 (1.9 + x + widthInUnits' l * defaultBoundingBoxWidth) (y - defaultBoundingBoxHeight))
        (renderedRBranch, _, rLoopbackConnections) = renderRight rightBranch lOrigin rOrigin _mapOfOrigins lLoopbackConnections
        connectionLX = x + defaultBoundingBoxWidth * 0.5
     in ( renderQuestion forkId origin content _mapOfOrigins
            <> renderedLBranch
            <> renderText "no" (x + defaultBoundingBoxWidth * 0.97) (y - defaultBoundingBoxHeight * 0.35)
            <> renderText "yes" (x + defaultBoundingBoxWidth * 0.42) (y - defaultBoundingBoxHeight * 0.9)
            <> case lDetourId of
              Nothing ->
                renderConnection
                  [ p2 (connectionLX, lY - heightInUnits' l * defaultBoundingBoxHeight),
                    p2 (connectionLX, y - heightInUnits fork * defaultBoundingBoxHeight)
                  ]
              Just _ -> mempty
            <> ( if null r
                   then
                     ( case rDetourId of
                         Nothing ->
                           renderConnection
                             [ p2 (x + defaultBoundingBoxWidth * (widthRatio + 1) / 2.0, y - defaultBoundingBoxHeight * 0.5),
                               p2 (rX - 0.1, y - defaultBoundingBoxHeight * 0.5),
                               p2 (rX - 0.1, rY - (if null l then 0.0 else defaultBoundingBoxHeight * 0.25))
                             ]
                         Just _ -> renderedRBranch
                     )
                   else
                     renderConnection
                       [ p2 (x + defaultBoundingBoxWidth * (widthRatio + 1) / 2.0, y - defaultBoundingBoxHeight * 0.5),
                         p2 (rX + defaultBoundingBoxWidth * 0.5, y - defaultBoundingBoxHeight * 0.5),
                         p2 (rX + defaultBoundingBoxWidth * 0.5, rY)
                       ]
                       <> renderedRBranch
               )
            <> position
              [ ( origin,
                  if troubleshootingMode
                    then
                      boundingBox
                        (widthInUnits fork * defaultBoundingBoxWidth)
                        (heightInUnits fork * defaultBoundingBoxHeight)
                    else mempty
                )
              ]
            <> case rDetourId of
              Nothing ->
                ( if null r
                    then
                      renderConnection
                        [ p2
                            ( rX - 0.1,
                              y
                                - defaultBoundingBoxHeight
                                  * ( if null l
                                        then 1.0
                                        else 1.25
                                    )
                            ),
                          p2 (rX - 0.1, y - heightInUnits fork * defaultBoundingBoxHeight),
                          p2 (x + defaultBoundingBoxWidth * 0.5, y - heightInUnits fork * defaultBoundingBoxHeight)
                        ]
                    else
                      renderConnection
                        [ p2 (rX + defaultBoundingBoxWidth * 0.5, y - defaultBoundingBoxHeight),
                          p2 (rX + defaultBoundingBoxWidth * 0.5, y - heightInUnits fork * defaultBoundingBoxHeight),
                          p2 (x + defaultBoundingBoxWidth * 0.5, y - heightInUnits fork * defaultBoundingBoxHeight)
                        ]
                )
              Just _ -> mempty,
          rLoopbackConnections
        )

widthInUnits :: SkewerBlock -> Double
widthInUnits (Fork _ _ _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r rId)) =
  ( if null l
      then 1.0
      else widthInUnits' l
  )
    + ( if null r
          then case rId of
            Nothing -> 0.0
            _ -> 1.0
          else widthInUnits' r
      )
widthInUnits _ = 1.0

heightInUnits :: SkewerBlock -> Double
heightInUnits (Fork _forkId _origin _ (ConnectedSkewerBlocks l _) (ConnectedSkewerBlocks r _)) =
  1.0
    + max
      ( if null l
          then 0.0
          else heightInUnits' l
      )
      ( if null r
          then 0.0
          else heightInUnits' r
      )
heightInUnits Headline {} = 1.0
heightInUnits _ = 1.0

blockHeightInUnits :: SkewerBlock -> Double
blockHeightInUnits Headline {} = 0.6
blockHeightInUnits Address {} = 0.6
blockHeightInUnits _ = 0.5

blockHeightOffsetInUnits :: SkewerBlock -> Double
blockHeightOffsetInUnits Address {} = 0.15
blockHeightOffsetInUnits _ = 0.25
