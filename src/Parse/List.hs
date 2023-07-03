module Parse.List ( childList
                  , findIndexStartWith ) where

import qualified Data.List as L

childList :: Eq a => a -> [[a]] -> Maybe [a]
childList searchString = L.find (elem searchString)

findIndexStartWith :: Eq a => [a] -> [[a]] -> Maybe Int
findIndexStartWith subchildPrefix = L.findIndex (L.isPrefixOf subchildPrefix)

