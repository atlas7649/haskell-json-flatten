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
    ["--unflatten", filePath] -> handleUnflatten (Just filePath)
    ["--unflatten"]           -> handleUnflatten Nothing
    ["--json", filePath]      -> handleJson (Just filePath)
    ["--json"]                -> handleJson Nothing
    [filePath]                 -> handleDefault (Just filePath)
    []                          -> handleDefault Nothing
    _ -> putStrLn "Usage: flatten-json-cli [--unflatten | --json] [file.json]"

handleUnflatten :: Maybe FilePath -> IO ()
handleUnflatten mPath = do
  content <- readInput mPath
  case decode content of
    Just (Object o) -> B.putStr (encode $ unflattenJSON "." o)
    Just _ -> putStrLn "Error: Unflattening requires a JSON object at the root"
    Nothing -> putStrLn "Error: Invalid JSON file"

handleJson :: Maybe FilePath -> IO ()
handleJson mPath = do
  content <- readInput mPath
  case decode content of
    Just val -> B.putStr (encode $ flattenJSON "." val)
    Nothing -> putStrLn "Error: Invalid JSON file"

handleDefault :: Maybe FilePath -> IO ()
handleDefault mPath = do
  content <- readInput mPath
  case decode content of
    Just val -> do
      let flattened = flattenJSON "." val
      let sortedItems = sortOn fst (HM.toList flattened)
      mapM_ (\(k, v) -> TIO.putStrLn $ k <> ": " <> T.pack (show v)) sortedItems
    Nothing -> putStrLn "Error: Invalid JSON file"

readInput :: Maybe FilePath -> IO B.ByteString
readInput (Just path) = B.readFile path
readInput Nothing     = B.hGetContents stdin