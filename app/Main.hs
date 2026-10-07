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

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--pretty" : rest) -> handleArgs True rest
    rest                  -> handleArgs False rest

handleArgs :: Bool -> [String] -> IO ()
handleArgs pretty args = case args of
    ("--delim" : d : "--unflatten" : filePath : _) -> handleUnflatten pretty (T.pack d) (Just filePath)
    ("--delim" : d : "--unflatten" : _)           -> handleUnflatten pretty (T.pack d) Nothing
    ("--delim" : d : "--json" : filePath : _)      -> handleJson pretty (T.pack d) (Just filePath)
    ("--delim" : d : "--json" : _)                -> handleJson pretty (T.pack d) Nothing
    ("--delim" : d : filePath : _)                 -> handleDefault pretty (T.pack d) (Just filePath)
    ("--delim" : d : _)                             -> handleDefault pretty (T.pack d) Nothing
    ["--unflatten", filePath] -> handleUnflatten pretty "." (Just filePath)
    ["--unflatten"]           -> handleUnflatten pretty "." Nothing
    ["--json", filePath]      -> handleJson pretty "." (Just filePath)
    ["--json"]                -> handleJson pretty "." Nothing
    [filePath]                 -> handleDefault pretty "." (Just filePath)
    []                          -> handleDefault pretty "." Nothing
    _ -> putStrLn "Usage: flatten-json-cli [--pretty] [--delim <char>] [--unflatten | --json] [file.json]"

handleUnflatten :: Bool -> T.Text -> Maybe FilePath -> IO ()
handleUnflatten pretty delim mPath = do
  content <- readInput mPath
  case decode content of
    Just (Object o) -> B.putStr (if pretty then encodePretty else encode $ unflattenJSON delim o)
    Just _ -> putStrLn "Error: Unflattening requires a JSON object at the root"
    Nothing -> putStrLn "Error: Invalid JSON file"

handleJson :: Bool -> T.Text -> Maybe FilePath -> IO ()
handleJson pretty delim mPath = do
  content <- readInput mPath
  case decode content of
    Just val -> B.putStr (if pretty then encodePretty else encode $ flattenJSON delim val)
    Nothing -> putStrLn "Error: Invalid JSON file"

handleDefault :: Bool -> T.Text -> Maybe FilePath -> IO ()
handleDefault _ delim mPath = do
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