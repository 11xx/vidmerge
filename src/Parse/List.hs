module Parse.List ( childList ) where

import qualified Data.List as L

childList :: Eq a => a -> [[a]] -> Maybe [a]
childList searchString = L.find (elem searchString)

