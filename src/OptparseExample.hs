module OptparseExample(someFunc9 ) where

import Options.Applicative

data Options = Options
  { optVerbose :: Bool
  , optInput   :: FilePath
  , optOutput  :: FilePath
  } deriving (Show, Eq)
  
options :: Parser Options
options = Options
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose mode" )
  <*> strOption
      ( long "input"
     <> short 'i'
     <> metavar "FILE"
     <> help "Input file" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file" )

someFunc9 :: IO ()
someFunc9 = do
  opts <- execParser (info (options <**> helper)
                     (fullDesc
                  <> progDesc "Process input file and write output"
                  <> header "Example program"))
  putStrLn $ "Options: " ++ show opts
