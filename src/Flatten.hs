{-# LANGUAGE OverloadedStrings #-}
module Flatten (flattenJSON, unflattenJSON, flattenJSONToList, unflattenJSONFromList, validateFlattened) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.List (foldl', sortOn, isPrefixOf)
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
    insertPath root (path, val)
      | T.null path = val
      | otherwise = go root (parsePath path)
      where
        go _ [] = val
        go (Object o) (p:ps) = 
          let existing = fromMaybe (Object HM.empty) (HM.lookup p o)
          in Object (HM.insert p (go existing ps) o)
        go _ (p:ps) = Object (HM.singleton p (go (Object HM.empty) ps))

    parsePath :: T.Text -> [T.Text]
    parsePath t 
      | T.null t = []
      | otherwise = 
          if "[" `T.isPrefixOf` t
          then let (idx, rest) = T.breakOn "]" (T.drop 1 t)
               in ("[" <> idx <> "]") : parsePath (T.drop 1 rest)
          else let (key, rest) = T.breakOn delim t
                   (keyNoIdx, idxPart) = T.breakOn "[" key
               in if not (T.null idxPart)
                  then let (idx, remainingKey) = T.breakOn "]" (T.drop 1 idxPart)
                       in keyNoIdx : ("[" <> idx <> "]") : parsePath (T.drop 1 remainingKey)
                  else if T.null rest
                       then [key]
                       else key : parsePath (T.drop (T.length delim) rest)

    finalize :: Value -> Value
    finalize (Object o) = 
      let processed = HM.map finalize o
          keys = HM.keys processed
          isArrayIndex k = T.length k >= 3 && T.head k == '[' && T.last k == ']'
          allIndices = not (null keys) && all isArrayIndex keys
          numericIndices = [ (readMaybe (T.unpack $ T.init $ T.tail k) :: Maybe Int, k) | k <- keys ]
          validIndices = all (\(m, _) -> m /= Nothing) numericIndices
      in if allIndices && validIndices
         then let sorted = sortOn fst [ (idx, v) | (Just idx, k) <- numericIndices, let v = processed HM.! k ]
              in Array (V.fromList $ map snd sorted)
         else Object processed
    finalize (Array a) = Array (V.map finalize a)
    finalize v = v

validateFlattened :: T.Text -> HM.HashMap T.Text Value -> Either T.Text ()
validateFlattened delim flattened = 
  let keys = HM.keys flattened
      collisions = [ (k1, k2) | k1 <- keys, k2 <- keys, k1 /= k2, isParentOf k1 k2 ]
  in if null collisions
     then Right ()
     else Left $ "Collision detected between paths: " <> T.pack (show $ head collisions)
  where
    isParentOf p1 p2 = 
      let p1' = p1 <> delim
      in p1' `T.isPrefixOf` p2