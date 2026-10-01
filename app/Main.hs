{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Flatten (flattenJSON)
import System.Environment (getArgs)
import Data.List (sortOn)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- B.readFile filePath
      case decode content of
        Just val -> do
          let flattened = flattenJSON val
          let sortedItems = sortOn fst (HM.toList flattened)
          mapM_ (\(k, v) -> TIO.putStrLn $ k <> ": " <> T.pack (show v)) sortedItems
        Nothing -> putStrLn "Error: Invalid JSON file"
    _ -> putStrLn "Usage: flatten-json-cli <file.json>"