{-# LANGUAGE OverloadedStrings #-}
module Flatten (flattenJSON) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Scientific (Scientific)
import qualified Data.Vector as V

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