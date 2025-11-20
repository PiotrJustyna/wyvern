module WyvernDiagram where

import qualified Blocks (Block (..), newRender, newRender')
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import Content (Content (Content))
import Data.HashSet (HashSet, empty)
import Data.Map (Map, empty)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram, Point (..), V2 (..), p2, position)
import EndTerminator (EndTerminator (End), changeOrigin, heightInUnits, render)
import HelperDiagrams (renderConnection, wyvernRect)
import ID (ID (ID))
import SkewerBlock (SkewerBlock, heightInUnits', position', render, renderIcons, toMap, widthInUnits')
import StartTerminator (StartTerminator (Title), changeOrigin, heightInUnits, render)

data WyvernDiagram
  = WyvernDiagram StartTerminator [[SkewerBlock]] EndTerminator
  deriving (Show)

data WyvernDiagram'
  = WyvernDiagram' [[Blocks.Block]]
  deriving (Show)

newRender :: WyvernDiagram' -> Diagram B
newRender (WyvernDiagram' bs) =
  let bs' = Blocks.StartTerminator : ((head bs) <> [Blocks.EndTerminator])
  in Blocks.newRender (bs' : [[Blocks.Action Nothing "huh"]])
