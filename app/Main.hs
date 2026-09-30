{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import Flatten (flattenJSON)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- B.readFile filePath
      case decode content of
        Just val -> do
          let flattened = flattenJSON val
          mapM_ (\(k, v) -> TIO.putStrLn $ k <> ": " <> T.pack (show v)) (HM.toList flattened)
        Nothing -> putStrLn "Error: Invalid JSON file"
    _ -> putStrLn "Usage: flatten-json-cli <file.json>"
  where
    import qualified Data.Text as T