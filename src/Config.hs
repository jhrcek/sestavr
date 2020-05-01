module Config
  ( Config (..),
    parseArgs,
  )
where

import Control.Monad (unless, when)
import qualified Model
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (die)

data Config = Config
  { configPort :: Int,
    configDbFile :: FilePath,
    configImagesDir :: FilePath,
    configCreateDemoData :: Bool,
    configConnectionPoolSize :: Int
  }

configParser :: Parser Config
configParser =
  Config
    <$> option
      auto
      ( long "port"
          <> metavar "INT"
          <> help "HTTP Server port number"
          <> value 3000
          <> showDefault
      )
    <*> strOption
      ( long "database-file"
          <> metavar "FILE"
          <> value "sestavr.db"
          <> showDefault
      )
    <*> strOption
      ( long "images-directory"
          <> metavar "DIRECTORY"
      )
    <*> switch
      ( long "create-demo-data"
          <> help "Whether to initialize the database with demo data"
      )
    <*> option
      auto
      ( long "pool-size"
          <> metavar "INT"
          <> help "Number of connections in the database connection pool"
          <> value 2
          <> showDefault
      )

parseArgs :: IO Config
parseArgs = ensureValid =<< execParser opts
  where
    opts =
      info
        (configParser <**> helper)
        ( fullDesc
            <> progDesc "application for planning yoga lessons"
            <> header "sestavr"
        )

ensureValid :: Config -> IO Config
ensureValid config = do
  imageDirExists <- doesDirectoryExist $ configImagesDir config
  unless imageDirExists $ die "Images directory does not exist"
  let dbFile = configDbFile config
      createDemoData = configCreateDemoData config
  dbFileExists <- doesFileExist dbFile
  when (createDemoData && dbFileExists) $ die $
    unlines
      [ "You asked me to initialize DB with demo data.",
        "But the database file '" ++ dbFile ++ "' already exists.",
        "Exiting, because I don't want to overwrite existing database"
      ]
  when createDemoData Model.createDemoData
  pure config
