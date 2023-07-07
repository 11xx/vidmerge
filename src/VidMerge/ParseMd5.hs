module VidMerge.ParseMd5 ( csvLines
                         , ptsToSec
                         , parseFrameIndexKnob
                         , findHeaderIndex
                         , readIntBS
                         , getPts
                         , readFloatBS ) where


import Data.ByteString (ByteString)
import Data.Knob (Knob)
import qualified Data.Knob as Knob

import qualified Data.ByteString.Char8 as C
import qualified Parse.ByteString.List as PL

import IO.Error ( exitWithErrorMsg )
import Text.Regex.TDFA ( (=~) )
import qualified Data.List as L
import Data.Maybe ( fromMaybe )
import Control.Monad ( when )

csvLines :: [ByteString] -> [[ByteString]]
csvLines = map (map C.strip . C.split ',')

ptsToSec :: Maybe Int -> [Int] -> Float
ptsToSec pts tb = case pts of
    Just ptsInt -> calc
      where
        fi = fromIntegral
        ptsFloat   = fi ptsInt    :: Float
        sec        = fi (head tb) :: Float
        frac       = fi (last tb) :: Float
        calc       = ptsFloat * sec / frac
    Nothing -> 0.0

getPts :: Maybe [ByteString] -> Maybe Int
getPts line = case line of
                 Just cl -> Just $ readIntBS (cl !! 2)
                 Nothing -> Nothing

readIntBS :: ByteString -> Int
readIntBS bs =
  case C.readInt bs of
    Just (n, _) -> n
    Nothing -> 1

readFloatBS :: ByteString -> Float
readFloatBS bs =
  case C.readInt bs of
    Just (n, _) -> fromIntegral n
    Nothing -> 1.0


parseFrameIndexKnob :: Knob -> IO ([ByteString], [Int], [ByteString])
parseFrameIndexKnob knob = do
  kc <- Knob.getContents knob
  when (C.null kc) $ exitWithErrorMsg "No frameindex contents found!"

  let headerIndex  = findHeaderIndex linesListAll
      linesListAll = C.lines kc
      linesList    = drop (headerIndex + 1) linesListAll
      tbLine       = linesListAll !! tbIndex
      tbIndex      = fromMaybe 0
                     $ PL.findIndexStartWith (C.pack "#tb") linesListAll
      tb           = map readIntBS (PL.lastTwoSplit '/' tbLine)
  when (tbIndex == 0) noTbError  -- 0 is the `Maybe' fallback

  pure (linesListAll, tb, linesList)

    where
      noTbError = exitWithErrorMsg $ unwords
        [ "No #tb (timebase) found in the header of frameindex, probably"
        , "the frameindex wasn't generated properly."
        ]

findHeaderIndex :: [ByteString] -> Int
findHeaderIndex = go 0
  where
    go index (line:rest)
      | headerRegex line = index
      | otherwise = go (index + 1) rest
    go _ [] = error "Header not found in CSV frameindex."
    headerRegex :: ByteString -> Bool
    headerRegex line =
      line =~ L.intercalate "[[:space:]]+"
      [ "#stream#,"
      , "dts,"
      , "pts,"
      , "duration,"
      , "size,"
      , "hash"
      ]

