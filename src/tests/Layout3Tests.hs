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
            h1MaxX `shouldBe` defaultBoundingBoxWidth
          it "should have correct boundary coordinate y" $
            h1MinY `shouldBe` defaultBoundingBoxHeight * (-1.0)

-- context "Fork overall size" $ do
--   it "max width should be 0.0" $
--     forkMaxX `shouldBe` defaultBoundingBoxWidth * 3.0
--   it "max depth should be 0.0" $
--     formMinY `shouldBe` defaultBoundingBoxHeight * (-4.0)

-- context "Fork left branch" $ do
--   let (lx, ly, lMaxX, lMinY) = getPosition l
--   it "should position left branch at x=0.0" $
--     lx `shouldBe` 0.0
--   it "should position left branch one level below fork (y=-1*boxHeight)" $
--     ly `shouldBe` (defaultBoundingBoxHeight * (-1.0))
--   it "should calculate correct maximum x coordinate" $
--     lMaxX `shouldBe` defaultBoundingBoxWidth
--   it "should calculate correct maximum y coordinate" $
--     lMinY `shouldBe` (defaultBoundingBoxHeight * (-2.0))

-- context "Fork right branch" $ do
--   let [(PositionedAction _i _c ax ay aMaxX aMinY), _f] = r
--   it "should correctly position right branch - x coordinate" $
--     ax `shouldBe` defaultBoundingBoxWidth
--   it "should correctly position right branch - y coordinate" $
--     ay `shouldBe` defaultBoundingBoxHeight * (-1.0)
--   it "should calculate correct maximum x coordinate" $
--     aMaxX `shouldBe` defaultBoundingBoxWidth * (2.0)
--   it "should calculate correct maximum y coordinate" $
--     aMinY `shouldBe` (defaultBoundingBoxHeight * (-2.0))
