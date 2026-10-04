{-# LANGUAGE OverloadedStrings #-}
module Flatten (flattenJSON, unflattenJSON) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List (foldl', sortOn)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

flattenJSON :: Value -> HM.HashMap T.Text Value
flattenJSON val = go "" val
  where
    go prefix (Object o) = 
      let items = HM.toList o
          flattenedItems = concatMap (\(k, v) -> 
            let newKey = if T.null prefix then k else prefix <> "." <> k
            in HM.toList $ go newKey v) items
      in HM.fromList flattenedItems
    go prefix (Array a) = 
      let items = V.toList a
          flattenedItems = concatMap (\(i, v) -> 
            let newKey = if T.null prefix then "[" <> T.pack (show i) <> "]" else prefix <> "[" <> T.pack (show i) <> "]"
            in HM.toList $ go newKey v) (zip [0..] items)
      in HM.fromList flattenedItems
    go prefix v = if T.null prefix then HM.empty else HM.singleton prefix v

unflattenJSON :: HM.HashMap T.Text Value -> Value
unflattenJSON flattened = finalize (foldl' insertPath (Object HM.empty) (HM.toList flattened))
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
    parsePath t = filter (not . T.null) $ foldr splitArray [t] [T.pack "["]
      where
        splitArray sep acc = concatMap (splitParts sep) acc
        splitParts sep part = 
          case T.breakOn sep part of
            (prefix, suffix) | T.null suffix -> [prefix]
            (prefix, suffix) -> 
              let rest = T.drop (T.length sep) suffix
                  (idx, remainder) = T.breakOn "]" rest
                  fullIdx = idx <> "]"
              in if T.null prefix then [fullIdx] <> splitParts sep remainder
                 else prefix : fullIdx : splitParts sep remainder

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