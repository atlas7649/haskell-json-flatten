{-# LANGUAGE OverloadedStrings #-}
module Flatten (flattenJSON, unflattenJSON, flattenJSONToList, unflattenJSONFromList) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List (foldl', sortOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

flattenJSON :: T.Text -> Value -> HM.HashMap T.Text Value
flattenJSON delim val = HM.fromList $ flattenJSONToList delim val

flattenJSONToList :: T.Text -> Value -> [(T.Text, Value)]
flattenJSONToList delim val = go "" val
  where
    go prefix (Object o) = 
      let items = HM.toList o
      in concatMap (\(k, v) -> 
            let newKey = if T.null prefix then k else prefix <> delim <> k
            in go newKey v) items
    go prefix (Array a) = 
      let items = V.toList a
      in concatMap (\(i, v) -> 
            let newKey = prefix <> "[" <> T.pack (show i) <> "]"
            in go newKey v) (zip [0..] items)
    go prefix v = if T.null prefix then [("", v)] else [(prefix, v)]

unflattenJSON :: T.Text -> HM.HashMap T.Text Value -> Value
unflattenJSON delim flattened = unflattenJSONFromList delim (HM.toList flattened)

unflattenJSONFromList :: T.Text -> [(T.Text, Value)] -> Value
unflattenJSONFromList delim flattened = finalize (foldl' insertPath (Object HM.empty) flattened)
  where
    insertPath :: Value -> (T.Text, Value) -> Value
    insertPath root (path, val) = go root (parsePath path)
      where
        go _ [] = val
        go (Object o) (p:ps) = 
          let existing = fromMaybe (Object HM.empty) (HM.lookup p o)
          in Object (HM.insert p (go existing ps) o)
        go _ (p:ps) = Object (HM.singleton p (go (Object HM.empty) ps))

    parsePath :: T.Text -> [T.Text]
    parsePath t = go t
      where
        go txt
          | T.null txt = []
          | "[" `T.isPrefixOf` txt = 
              let (idx, rest) = T.breakOn "]" txt
              in if T.null rest 
                 then [idx <> "]"] 
                 else (idx <> "]") : go (T.drop 1 rest)
          | otherwise = 
              let (prefix, suffix) = T.breakOn delim txt
              in if T.null suffix
                 then [prefix]
                 else prefix : go (T.drop (T.length delim) suffix)

    finalize :: Value -> Value
    finalize (Object o) = 
      let processed = HM.map finalize o
          keys = HM.keys processed
          -- Check if all keys are of the form "[i]"
          isArrayIndex k = T.length k >= 3 && T.head k == '[' && T.last k == ']'
          allIndices = all isArrayIndex keys
          numericIndices = [ (readMaybe (T.unpack $ T.init $ T.tail k) :: Maybe Int, k) | k <- keys ]
          validIndices = all (\(m, _) -> m /= Nothing) numericIndices
      in if allIndices && validIndices
         then let sorted = sortOn fst [ (idx, v) | (Just idx, k) <- numericIndices, let v = processed HM.! k ]
              in Array (V.fromList $ map snd sorted)
         else Object processed
    finalize (Array a) = Array (V.map finalize a)
    finalize v = v