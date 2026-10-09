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
    Left err -> putStrLn err >> printUsage
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
runCLI cfg
  | cfgUnflatten cfg = handleUnflatten cfg
  | cfgJson cfg     = handleJson cfg
  | otherwise       = handleDefault cfg

handleUnflatten :: Config -> IO ()
handleUnflatten cfg = do
  content <- readInput (cfgFile cfg)
  case decode content of
    Just (Object o) -> B.putStr (if cfgPretty cfg then encodePretty else encode $ unflattenJSON (cfgDelim cfg) o)
    Just _ -> putStrLn "Error: Unflattening requires a JSON object at the root"
    Nothing -> putStrLn "Error: Invalid JSON file"

handleJson :: Config -> IO ()
handleJson cfg = do
  content <- readInput (cfgFile cfg)
  case decode content of
    Just val -> B.putStr (if cfgPretty cfg then encodePretty else encode $ flattenJSON (cfgDelim cfg) val)
    Nothing -> putStrLn "Error: Invalid JSON file"

handleDefault :: Config -> IO ()
handleDefault cfg = do
  content <- readInput (cfgFile cfg)
  case decode content of
    Just val -> do
      let flattened = flattenJSON (cfgDelim cfg) val
      let sortedItems = sortOn fst (HM.toList flattened)
      mapM_ (\(k, v) -> TIO.putStrLn $ k <> ": " <> T.pack (show v)) sortedItems
    Nothing -> putStrLn "Error: Invalid JSON file"

readInput :: Maybe FilePath -> IO B.ByteString
readInput (Just path) = B.readFile path
readInput Nothing     = B.hGetContents stdin