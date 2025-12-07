module InputArguments where

import Options.Applicative (Parser, help, long, metavar, short, strOption)

data InputArguments = InputArguments
  { inputPath :: String,
    outputPath :: String
  }

instance Show InputArguments where
  show x =
    let userProvidedInputPath = inputPath x
        userProvidedOutputPath = outputPath x
     in "input path: \""
          <> userProvidedInputPath
          <> "\"\n"
          <> "output path: \""
          <> userProvidedOutputPath
          <> "\""

parseInput :: Parser InputArguments
parseInput =
  InputArguments
    <$> strOption
      ( long "inputPath"
          <> short 'i'
          <> metavar "PATH"
          <> help "Path to input *.txt drakon diagram file."
      )
    <*> strOption
      ( long "outputPath"
          <> short 'o'
          <> metavar "PATH"
          <> help "Path to output *.svg drakon diagram file."
      )
