{-# LANGUAGE OverloadedStrings #-}
module Flatten (flattenJSON, unflattenJSON) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List (foldl')
import Data.Maybe (fromMaybe)

flattenJSON :: Value -> HM.HashMap T.Text Value
flattenJSON = go ""
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
            let newKey = prefix <> "[" <> T.pack (show i) <> "]"
            in HM.toList $ go newKey v) (zip [0..] items)
      in HM.fromList flattenedItems
    go prefix val = HM.singleton prefix val

unflattenJSON :: HM.HashMap T.Text Value -> Value
unflattenJSON flattened = foldl' insertPath (Object HM.empty) (HM.toList flattened)
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
    parsePath t = T.splitOn "." t