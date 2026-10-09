{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Flatten (flattenJSON, unflattenJSON)
import System.Environment (getArgs)
import Data.List (sortOn)
import System.IO (stdin)
import System.Directory (doesFileExist)

data Config = Config
  { cfgPretty    :: Bool
  , cfgDelim     :: T.Text
  , cfgUnflatten :: Bool
  , cfgJson     :: Bool
  , cfgFile      :: Maybe FilePath
  }

defaultConfig :: Config
defaultConfig = Config False "." False False Nothing

main :: IO ()
main = do
  args <- getArgs
  case parseArgs defaultConfig args of
    Left err -> if err == "HELP" then printUsage else putStrLn err >> printUsage
    Right cfg -> runCLI cfg

printUsage :: IO ()
printUsage = putStrLn "Usage: flatten-json-cli [OPTIONS] [file.json]

Options:
  --pretty            Print JSON output with indentation
  --delim <char>      Use a custom delimiter (default: '.')
  --unflatten          Unflatten a flat JSON map back to nested structure
  --json               Output the flattened result as JSON instead of text
  --help              Show this help message

Example:
  flatten-json-cli input.json
  flatten-json-cli --pretty --delim "/" --json input.json
  flatten-json-cli --unflatten flat.json"

parseArgs :: Config -> [String] -> Either String Config
parseArgs cfg [] = Right cfg
parseArgs cfg ("--help":_) = Left "HELP"
parseArgs cfg ("--pretty":rest) = parseArgs cfg { cfgPretty = True } rest
parseArgs cfg ("--unflatten":rest) = parseArgs cfg { cfgUnflatten = True } rest
parseArgs cfg ("--json":rest) = parseArgs cfg { cfgJson = True } rest
parseArgs cfg ("--delim":d:rest) = parseArgs cfg { cfgDelim = T.pack d } rest
parseArgs cfg ("--delim":[]) = Left "--delim requires an argument"
parseArgs cfg (path:rest) 
  | "--" `T.isPrefixOf` T.pack path = Left $ "Unknown option: " ++ path
  | otherwise = case cfgFile cfg of
      Nothing -> parseArgs cfg { cfgFile = Just path } rest
      Just _  -> Left "Multiple input files provided"

runCLI :: Config -> IO ()
runCLI cfg = do
  inputResult <- readInput (cfgFile cfg)
  case inputResult of
    Left err -> putStrLn $ "Error: " ++ err
    Right content -> 
      case decode content of
        Nothing -> putStrLn "Error: Invalid JSON content"
        Just val -> executeAction cfg val

executeAction :: Config -> Value -> IO ()
executeAction cfg val
  | cfgUnflatten cfg = case val of
      Object o -> B.putStr (if cfgPretty cfg then encodePretty else encode $ unflattenJSON (cfgDelim cfg) o)
      _        -> putStrLn "Error: Unflattening requires a JSON object at the root"
  | cfgJson cfg = B.putStr (if cfgPretty cfg then encodePretty else encode $ flattenJSON (cfgDelim cfg) val)
  | otherwise = do
      let flattened = flattenJSON (cfgDelim cfg) val
      let sortedItems = sortOn fst (HM.toList flattened)
      mapM_ (\(k, v) -> TIO.putStrLn $ k <> ": " <> T.pack (show v)) sortedItems

readInput :: Maybe FilePath -> IO (Either String B.ByteString)
readInput (Just path) = do
  exists <- doesFileExist path
  if exists
    then Right <$> B.readFile path
    else return $ Left $ "File not found: " ++ path
readInput Nothing = Right <$> B.hGetContents stdin