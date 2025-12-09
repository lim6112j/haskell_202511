module OptparseExample(someFunc9 ) where

import Options.Applicative

data Options = Options
  { optVerbose :: Bool
  , optInput   :: Maybe FilePath
  , optOutput  :: Maybe FilePath
  , optArgs    :: [String]
  } deriving (Show, Eq)
  
options :: Parser Options
options = Options
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose mode" )
  <*> optional (strOption
      ( long "input"
     <> short 'i'
     <> metavar "FILE"
     <> help "Input file" ))
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file" ))
  <*> many (argument str (metavar "ARGS..."))

someFunc9 :: IO ()
someFunc9 = do
  opts <- execParser (info (options <**> helper)
                     (fullDesc
                  <> progDesc "Process input file and write output"
                  <> header "Example program"))
  putStrLn $ "Options: " ++ show opts
  case optArgs opts of
    [] -> putStrLn "No arguments provided"
    args -> putStrLn $ "Arguments: " ++ unwords args
