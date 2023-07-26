module Parse.List ( childList
                  , childListAt
                  , findIndexStartWith
                  , findShortestMatch
                  ) where

import qualified Data.List as L
import qualified Safe
import Data.Maybe ( fromMaybe )

childList :: Eq a => a -> [[a]] -> Maybe [a]
childList searchString = L.find (elem searchString)

findIndexStartWith :: Eq a => [a] -> [[a]] -> Maybe Int
findIndexStartWith subchildPrefix = L.findIndex (L.isPrefixOf subchildPrefix)

childListAt :: (Eq a, Monoid a) => Int -> [[a]] -> [[a]] -> Maybe [a]
childListAt i ll1 = childList (fromMaybe mempty (fromMaybe mempty (ll1 `Safe.atMay` i) `Safe.atMay` 5))

findShortestMatch :: (Eq a, Monoid a) => Int -> Int -> [[a]] -> [[a]] -> Maybe [a]
findShortestMatch i maxI ll1 ll2
  | i >= maxI = Nothing
  | otherwise = case childListAt i ll1 ll2 of
      Just l -> Just l
      Nothing -> findShortestMatch (i + 1) maxI ll1 ll2
