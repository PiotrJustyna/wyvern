module Layout3Tests where

import Blocks
import Constants (defaultBoundingBoxHeight, defaultBoundingBoxWidth)
import ID
import Layout
import Lexer (lexAll, runAlex)
import Parser (ParseResult (..), diagram)
import PositionedBlock
import Test.Hspec

parseTextDiagram :: String -> Either String [[Block]]
parseTextDiagram textInput =
  case runAlex textInput lexAll of
    Left lexingError -> Left $ "Lexing error: " <> lexingError
    Right tokens ->
      case diagram tokens 1 of
        ParseOk skewers -> Right skewers
        ParseFail parseError -> Left $ "Parse error: " <> parseError

diagramFromFile :: FilePath -> IO (Either String [[Block]])
diagramFromFile filePath = do
  content <- readFile filePath
  pure $ parseTextDiagram content

specLayout3 :: Spec
specLayout3 = describe "layout3" $ do
  result <- runIO (diagramFromFile "diagrams/tests/Layout3.txt")
  case result of
    Left err -> error $ "Failed to parse test diagram from file: " <> err
    Right blocks -> do
      let [((PositionedHeadline _i _c _x _y h1MaxX h1MinY) : _)] = position (Blocks.reverse blocks) 0.0 0.0

      context "Boundary tests:" $ do
        context "Headline" $ do
          it "should have correct boundary coordinate x" $
            h1MaxX `shouldBe` defaultBoundingBoxWidth * 0.5
          it "should have correct boundary coordinate y" $
            h1MinY `shouldBe` defaultBoundingBoxHeight * (-1.0)
