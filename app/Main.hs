module Main (main) where

import VidMerge.FrameMd5
    ( frameIndexFromFile,
      makeFrameIndexExtension,
      newKnobFileOrOutput,
      writeKnobToFile )
import VidMerge.ParseMd5
    ( csvLines, ptsToSec, parseFrameIndexKnob )
import VidMerge.FpsProbe ( fpsProbe )
import Options
    ( execParser, Opts(Opts, optInputFile1), optsParserInfo )
import Parse.List (childList)
import qualified Parse.ByteString.List as PL
import System.Directory ( doesFileExist, makeAbsolute, removeFile )
import Text.Printf ( printf )
import Control.Monad ( when )
import qualified Data.List as L
import IO.Error ( exitWithErrorMsg )
import qualified Data.ByteString.Char8 as C
import System.IO.Temp ( writeSystemTempFile )
import System.FilePath ( dropExtensions, takeExtensions )
import System.Process ( callProcess )
import System.IO ( hPutStrLn, stderr )

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  concatFile <- frameIndexFromArgs opts
  (fps1, fps2) <- fpsProbePrint opts
  tempFile <- writeSystemTempFile "vidmerge-concatfile.txt" concatFile
  let outFile = dropExtensions (optInputFile1 opts)
                ++ "-merged"
                ++ takeExtensions (optInputFile1 opts)
  callProcess "ffmpeg"
    [ "-y", "-stats", "-hide_banner", "-avoid_negative_ts"
    , "make_zero", "-fflags", "+genpts", "-protocol_whitelist"
    , "file,pipe", "-f", "concat", "-safe", "0", "-i", tempFile
    , "-c", "copy", outFile
    ]
  hPutStrLn stderr "Removing temp file..."
  removeFile tempFile


frameIndexFromArgs :: Opts -> IO String
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
      lastFrameFile2
        | not $ null l = Just l
        | otherwise = Nothing
        where
          l = last csv2

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
      secOutF2 = ptsToSec lastFrameFile2 tb2

  when (secOutF1 == 0.0 || secInF2 == 0.0) noHashMatchErr

  pure $ printf format f1a secOutF1 f2a secInF2 secOutF2

  where
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
             , "inpoint 0.0\n"
             , "outpoint %.5f\n"
             , "file '%s'\n"
             , "inpoint %.5f\n"
             , "outpoint %.5f\n"
             ]

fpsProbePrint :: Opts -> IO (Int, Int)
fpsProbePrint (Opts f1 f2 _) = do
  pf1 <- fpsProbe f1
  pf2 <- fpsProbe f2
  pure (ri $ splitHead pf1, ri $ splitHead pf2)
    where
      splitHead = head . C.split '/' . C.pack
      ri bs = case C.readInt bs of
                Just (int, _) -> int
                Nothing -> 0
