module Main (main) where

import VidMerge.FrameMd5
    ( writeKnobToFile,
      newKnobFileOrOutput,
      makeFrameIndexExtension,
      frameIndexFromFile )
import VidMerge.ParseMd5
    ( csvLines, ptsToSec, getPts, parseFrameIndexKnob, cleanKnob )
import VidMerge.FpsProbe ( fpsProbePrint )

import Options
    ( optsParserInfo,
      execParser,
      Opts(optVerbose, optInputFile2, optMatchShortest, optInputFile1) )

import System.Directory ( doesFileExist, makeAbsolute, removeFile )
import Control.Monad ( when, unless )
import qualified Data.List as L
import System.IO.Temp ( writeSystemTempFile )
import System.FilePath ( dropExtensions, takeExtensions )
import System.Process ( callProcess )
import System.IO ( hPutStrLn, stderr, hPrint )
import Data.ByteString (ByteString)
import Data.Maybe ( fromMaybe )
import Numeric ( showFFloat )
import Safe ( atMay )
import Version ( progName )
import Text.Printf ( printf )

import Parse.List ( childList, findShortestMatch )
import IO.Error ( exitWithErrorMsg, exitWithErrorMsgNoHashMatch )
import IO.Debug ( debugMsgLn, debugMsg )
import IO.Prompt ( promptContinueMergedFileExists )

main :: IO ()
main = do
  opts <- execParser optsParserInfo

  (fa, _, secInLatest, secOut, csv1) <- frameIndexInOut (optInputFile1 opts) True Nothing
  (fa2, secOutShortestFile1, secInLatest2, secOut2, csv2) <- frameIndexInOut (optInputFile2 opts) False (Just csv1)

  when (optVerbose opts) $ do
    debugMsg "This is csv2 !! 5 childlist csv1: "
    hPrint stderr $ childList (fromMaybe mempty (head csv2 `Safe.atMay` 5)) csv1

    debugMsg "head csv2: "
    hPrint stderr $ head csv2
    debugMsgLn $ "Shortest: " <> printf "%.2f" secOutShortestFile1

    debugMsg "Find shortest match: "
    hPrint stderr $ findShortestMatch 0 (length csv1) csv2 csv1

  (_, fps2) <- fpsProbePrint (optInputFile1 opts) (optInputFile2 opts)

  let concatFile = formattedFile
        fa
        secInLatest
        secOutFile1
        fa2
        ( secInFile2 - ((1 / fromIntegral fps2)*2) )
        secOut2
      formattedFile f i o f2 i2 o2 = L.intercalate ""
        [ "file '",    f,         "'\n"
        , "inpoint ",  sf i  "",  "\n"
        , "outpoint ", sf o  "",  "\n"
        , "file '",    f2,        "'\n"
        , "inpoint ",  sf i2 "",  "\n"
        , "outpoint ", sf o2 "",  "\n"
        ]
      sf = showFFloat (Just 5)
      (secOutFile1, secInFile2)
        | optMatchShortest opts = (secOutShortestFile1, 0.0)
        | otherwise             = (secOut, secInLatest2)


  tempFile <- writeSystemTempFile (progName ++ "-concatfile.txt") concatFile
  let outFile = dropExtensions (optInputFile1 opts)
                ++ "-merged"
                ++ takeExtensions (optInputFile1 opts)
  doesOutMergedExist <- doesFileExist outFile
  when doesOutMergedExist $ do
    promptContinueMergedFileExists outFile
  -- TODO check file size of merged

  callProcess "ffmpeg"
    [ "-y", "-loglevel", "warning", "-stats", "-hide_banner", "-avoid_negative_ts"
    , "make_zero", "-fflags", "+genpts", "-protocol_whitelist"
    , "file,pipe", "-f", "concat", "-safe", "0", "-i", tempFile
    , "-c", "copy", outFile
    ]
  hPutStrLn stderr "Removing temp file..."

  unless (optVerbose opts) $ removeFile tempFile

frameIndexInOut
  :: FilePath -> Bool -> Maybe [[ByteString]]  -- args
  -> IO ( FilePath, Float, Float, Float, [[ByteString]] )  -- return
frameIndexInOut video isFirst csvPrevFile = do
  let frameIndexOutFile = makeFrameIndexExtension video

  (tbLine, linesList) <- frameIndexTbAndListLines video frameIndexOutFile

  let currentCsvList = csvLines linesList

  fileAbsolute <- makeAbsolute video

  let lastFrameLine
        | not $ null l = Just l
        | otherwise = Nothing
        where
          l = last currentCsvList

      lastPts = getPts lastFrameLine

  case lastFrameLine of
    Just _ -> pure ()
    Nothing -> exitWithErrorMsg "No last csv frameindex line retrieved."

  let lastMd5PrevLine = fromMaybe mempty
                        $ last
                        ( fromMaybe [[mempty]] csvPrevFile ) `Safe.atMay` 5
  let secOut = ptsToSec lastPts tbLine
  let secInMay
        | not isFirst = Just $ (`ptsToSec` tbLine) . getPts . childList lastMd5PrevLine $ currentCsvList
        | otherwise   = Nothing
      secInLatest = fromMaybe 0.0 secInMay
      secInShortestPrevFile = (`ptsToSec` tbLine) . getPts $ findShortestMatch 0 (length csvPrevFromMaybe) currentCsvList csvPrevFromMaybe
        where
          csvPrevFromMaybe = fromMaybe mempty csvPrevFile

  when (secOut == 0.0) exitWithErrorMsgNoHashMatch

  pure (fileAbsolute, secInShortestPrevFile, secInLatest, secOut, currentCsvList)


frameIndexTbAndListLines :: FilePath -> FilePath -> IO ([Int], [ByteString])
frameIndexTbAndListLines vid outFile = do
  oExist <- doesFileExist outFile
  kf <- newKnobFileOrOutput oExist outFile (Just $ frameIndexFromFile vid)
  writeKnobToFile oExist outFile kf
  (_, tbLine, linesList) <- parseFrameIndexKnob kf
  cleanKnob kf  -- testing, is replacing a Knob with [] the same as cleaning?
  pure (tbLine, linesList)
