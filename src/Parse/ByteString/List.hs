module Parse.ByteString.List ( findIndexStartWith
                             , lastTwoSplit
                             , childList ) where

import qualified Data.List as L
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS

findIndexStartWith :: ByteString -> [ByteString] -> Maybe Int
findIndexStartWith subchildPrefix = L.findIndex (BS.isPrefixOf subchildPrefix)

splitWords :: Char -> ByteString -> [[ByteString]]
splitWords c = map C.words . C.split c

lastTwoSplit :: Char -> ByteString -> [ByteString]
lastTwoSplit c s
  | null w = []
  | otherwise = [ last $ w !! (len - 2), head $ w !! (len - 1) ]
  where
    w = splitWords c s
    len = length w

childList :: ByteString -> [[ByteString]] -> Maybe [ByteString]
childList searchString = L.find (elem searchString)
