{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Flatten (flattenJSON, unflattenJSON)
import System.Environment (getArgs)
import Data.List (sortOn)
import System.IO (stdin)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--delim" : d : "--unflatten" : filePath : _) -> handleUnflatten (T.pack d) (Just filePath)
    ("--delim" : d : "--unflatten" : _)           -> handleUnflatten (T.pack d) Nothing
    ("--delim" : d : "--json" : filePath : _)      -> handleJson (T.pack d) (Just filePath)
    ("--delim" : d : "--json" : _)                -> handleJson (T.pack d) Nothing
    ("--delim" : d : filePath : _)                 -> handleDefault (T.pack d) (Just filePath)
    ("--delim" : d : _)                             -> handleDefault (T.pack d) Nothing
    ["--unflatten", filePath] -> handleUnflatten "." (Just filePath)
    ["--unflatten"]           -> handleUnflatten "." Nothing
    ["--json", filePath]      -> handleJson "." (Just filePath)
    ["--json"]                -> handleJson "." Nothing
    [filePath]                 -> handleDefault "." (Just filePath)
    []                          -> handleDefault "." Nothing
    _ -> putStrLn "Usage: flatten-json-cli [--delim <char>] [--unflatten | --json] [file.json]"

handleUnflatten :: T.Text -> Maybe FilePath -> IO ()
handleUnflatten delim mPath = do
  content <- readInput mPath
  case decode content of
    Just (Object o) -> B.putStr (encode $ unflattenJSON delim o)
    Just _ -> putStrLn "Error: Unflattening requires a JSON object at the root"
    Nothing -> putStrLn "Error: Invalid JSON file"

handleJson :: T.Text -> Maybe FilePath -> IO ()
handleJson delim mPath = do
  content <- readInput mPath
  case decode content of
    Just val -> B.putStr (encode $ flattenJSON delim val)
    Nothing -> putStrLn "Error: Invalid JSON file"

handleDefault :: T.Text -> Maybe FilePath -> IO ()
handleDefault delim mPath = do
  content <- readInput mPath
  case decode content of
    Just val -> do
      let flattened = flattenJSON delim val
      let sortedItems = sortOn fst (HM.toList flattened)
      mapM_ (\(k, v) -> TIO.putStrLn $ k <> ": " <> T.pack (show v)) sortedItems
    Nothing -> putStrLn "Error: Invalid JSON file"

readInput :: Maybe FilePath -> IO B.ByteString
readInput (Just path) = B.readFile path
readInput Nothing     = B.hGetContents stdin