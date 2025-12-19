module WyvernDiagram where

import qualified Blocks (Block (..), newRender', newRender1)
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Content (Content (Content))
import Data.HashSet (HashSet, empty)
import Data.Map (Map, empty)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position)
import ID (ID (ID))

newtype WyvernDiagram'
  = WyvernDiagram' [[Blocks.Block]]
  deriving (Show)

newRender1 :: WyvernDiagram' -> Diagram B
newRender1 (WyvernDiagram' bs) =
  let bs' = case bs of
        [] -> mempty
        [b] -> [(Blocks.StartTerminator : b) <> [Blocks.EndTerminator]]
        _ ->
          let bs' = tail bs
           in ((Blocks.StartTerminator : head bs) : init bs') <> [last bs' <> [Blocks.EndTerminator]]
   in Blocks.newRender1 bs'
