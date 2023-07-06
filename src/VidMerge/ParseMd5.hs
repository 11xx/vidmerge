module VidMerge.ParseMd5 ( csvLines
                         , ptsToSec
                         , parseFrameIndexKnob
                         , findHeaderIndex ) where


import Data.ByteString (ByteString)
import Data.Knob (Knob)
import qualified Data.Knob as Knob

import qualified Data.ByteString.Char8 as C
import qualified Parse.ByteString.List as PL

import Control.Monad.IO.Class ( MonadIO )


csvLines :: [ByteString] -> [[ByteString]]
csvLines = map (map C.strip . C.split ',')

ptsToSec :: Maybe [ByteString] -> [ByteString] -> Float
ptsToSec hashLine tb =   case hashLine of
    Just cl -> calc
      -- print cl
      where
        r = readIntBS
        pts        = r $ cl !! 2
        sec        = r $ head tb
        frac       = r $ last tb
        calc       = pts * sec / frac
        -- calcString = showFFloat (Just 4) calc []
    Nothing -> 0.0

readIntBS :: ByteString -> Float
readIntBS bs =
  case C.readInt bs of
    Just (n, _) -> fromIntegral n
    Nothing -> 1.0

parseFrameIndexKnob :: Control.Monad.IO.Class.MonadIO m
                    => Knob
                    -> m ( [ByteString]
                         , Maybe Int
                         , [ByteString]
                         )
parseFrameIndexKnob knob = do
  kc <- Knob.getContents knob
  let linesListAll = C.lines kc
      tbIndex      = PL.findIndexStartWith (C.pack "#tb") linesListAll
      headerIndex  = findHeaderIndex linesListAll
      -- headerLine   = linesListAll !! headerIndex
      linesList    = drop (headerIndex + 1) linesListAll
  pure (linesListAll, tbIndex, linesList)

findHeaderIndex :: [C.ByteString] -> Int
findHeaderIndex = go 0
  where
    go index (line:rest)
      | C.isPrefixOf (C.pack "#stream#, dts,        pts, duration,     size, hash") line = index
      | otherwise = go (index + 1) rest
    go _ [] = error "Header not found in CSV frameindex."
