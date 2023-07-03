module Parse.ByteString.List (findIndexStartWith) where

import qualified Data.List as L
import qualified Data.ByteString as BS

findIndexStartWith :: BS.ByteString -> [BS.ByteString] -> Maybe Int
findIndexStartWith subchildPrefix = L.findIndex (BS.isPrefixOf subchildPrefix)
