module Main (main) where

import VidMerge.FrameMd5
import VidMerge.ParseMd5
import Options ( optsParserInfo, execParser, Opts(Opts) )
import Parse.List (childList)
import qualified Parse.ByteString.List as PL
import System.Directory ( doesFileExist, makeAbsolute )
import Text.Printf ( printf )
import Control.Monad ( when )
import qualified Data.List as L
import IO.Error

-- import VidMerge.FpsProbe ( fpsProbe )
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as BS
-- import Data.Knob (Knob)
-- import qualified Data.Knob as Knob
-- import qualified Data.ByteString.Char8 as C
-- import System.IO ( hPutStr, hPutStrLn, stderr )
-- import System.Exit ( exitFailure )
-- import System.Console.ANSI
--     ( hSetSGR,
--       Color(Red, Green),
--       ColorIntensity(Vivid),
--       ConsoleLayer(Foreground),
--       SGR(Reset, SetColor) )
-- import Control.Monad.IO.Class ( MonadIO )
-- import Numeric ( showFFloat )

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  frameIndexFromArgs opts
  -- fpsProbePrint opts

frameIndexFromArgs :: Opts -> IO ()
frameIndexFromArgs (Opts f1 f2 _) = do
  let o1 = makeFrameIndexExtension f1
      o2 = makeFrameIndexExtension f2

  -- file1
  oExist <- doesFileExist o1
  kf <- newKnobFileOrOutput oExist o1 (Just $ frameIndexFromFile f1)
  writeKnobToFile oExist o1 kf
  (llall, tbi, ll) <- parseFrameIndexKnob kf

  -- file2
  oExist2 <- doesFileExist o2
  kf2 <- newKnobFileOrOutput oExist2 o2 (Just $ frameIndexFromFile f2)
  writeKnobToFile oExist2 o2 kf2
  (llall', tbi', ll') <- parseFrameIndexKnob kf2

  let csv1 = csvLines ll
      lastFrameFile1
        | not $ null l = Just l
        | otherwise = Nothing
        where
          l = last csv1

  let csv2 = csvLines ll'
      lastMatchFile2 = childList (last csv1 !! 5) csv2  -- 6th item

  tb <- case tbi of
          Just i -> pure $ llall !! i
          Nothing -> do noTbError

  tbLine <- case tbi' of
           Just i -> pure $ llall' !! i
           Nothing -> do noTbError

  let tb1 = PL.lastTwoSplit '/' tb
  let tb2 = PL.lastTwoSplit '/' tbLine

  f1a <- makeAbsolute f1
  f2a <- makeAbsolute f2
  let secOutF1 = ptsToSec lastFrameFile1 tb1
      secInF2 = ptsToSec lastMatchFile2 tb2

  when (secOutF1 == 0.0 || secInF2 == 0.0) noHashMatchErr

  printf format f1a secOutF1 f2a secInF2

  where
    -- oMsg f = unwords ["Output has been", "written", "to", f]  -- testing unwords
    noTbError = exitWithErrorMsg $ unwords
      [ "No #tb (timebase) found in the header of frameindex, probably"
      , "the frameindex wasn't generated properly."
      ]
    noHashMatchErr = exitWithErrorMsg $ unwords
        [ "No matching list containing hash."
        , "The second video may need fixup, is encoded differently or"
        , "is not the continuation of the first."
        ]
    format = L.intercalate ""
             [ "file '%s'\n"
             , "outpoint %.5f\n"
             , "file '%s'\n"
             , "inpoint %.5f\n"
             ]

-- fpsProbePrint :: Opts -> IO ()
-- fpsProbePrint (Opts f1 f2 _) = do
--   pf1 <- fpsProbe f1
--   putStr $ f1 ++ " file has fps: "
--   C.putStrLn $ splitHead pf1

--   pf2 <- fpsProbe f2
--   putStr $ f2 ++ " file has fps: "
--   C.putStrLn $ splitHead pf2
--     where
--       splitHead = head . C.split '/' . C.pack

-- ffmpeg -y -stats -hide_banner -avoid_negative_ts make_zero -fflags +genpts -protocol_whitelist file,pipe -f concat -safe 0 -i concatfile -c copy file.out.mp4
