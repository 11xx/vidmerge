module Main (main) where

import VidMerge.FrameMd5
    ( frameIndexFromFile,
      makeFrameIndexExtension,
      newKnobFileOrOutput,
      writeKnobToFile )
import VidMerge.ParseMd5
    ( csvLines, ptsToSec, getPts, readIntBS, parseFrameIndexKnob )

import VidMerge.FpsProbe ( fpsProbe )
import Options
    ( execParser, Opts(optInputFile1, optInputFile2), optsParserInfo )
import Parse.List (childList)
import System.Directory ( doesFileExist, makeAbsolute, removeFile )
import Control.Monad ( when )
import qualified Data.List as L
import IO.Error ( exitWithErrorMsg )
import qualified Data.ByteString.Char8 as C
import System.IO.Temp ( writeSystemTempFile )
import System.FilePath ( dropExtensions, takeExtensions )
import System.Process ( callProcess )
import System.IO
    ( hGetEcho,
      hSetBuffering,
      hSetEcho,
      hPutStrLn,
      stderr,
      stdin,
      BufferMode(NoBuffering) )
import Data.ByteString (ByteString)
import Data.Maybe ( fromMaybe )
import Numeric ( showFFloat )
import qualified Safe
import System.Exit ( exitFailure )

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  (fa, secIn, secOut, csv1) <- frameIndexInOut (optInputFile1 opts) True Nothing
  (fa2, secIn2, secOut2, _) <- frameIndexInOut (optInputFile2 opts) False (Just csv1)
  (_, fps2) <- fpsProbePrint (optInputFile1 opts) (optInputFile2 opts)
  -- let pad = optPadding opts
  let concatFile = fmt fa secIn secOut
                   fa2 (secIn2 - ((1 / fromIntegral fps2)*2)) secOut2
        where
          fmt f i o f2 i2 o2 = L.intercalate ""
            [ "file '", f, "'\n"
            , "inpoint ", showFFloat (Just 5) i "", "\n"
            , "outpoint ", showFFloat (Just 5) o "", "\n"
            , "file '", f2, "'\n"
            , "inpoint ", showFFloat (Just 5) i2 "", "\n"
            , "outpoint ", showFFloat (Just 5) o2 "", "\n"
            ]

  tempFile <- writeSystemTempFile "vidmerge-concatfile.txt" concatFile
  let outFile = dropExtensions (optInputFile1 opts)
                ++ "-merged"
                ++ takeExtensions (optInputFile1 opts)
  doesOutMergedExist <- doesFileExist outFile
  when doesOutMergedExist $ do
    promptAndContinue outFile
  -- TODO check file size of merged

  callProcess "ffmpeg"
    [ "-y", "-stats", "-hide_banner", "-avoid_negative_ts"
    , "make_zero", "-fflags", "+genpts", "-protocol_whitelist"
    , "file,pipe", "-f", "concat", "-safe", "0", "-i", tempFile
    , "-c", "copy", outFile
    ]
  hPutStrLn stderr "Removing temp file..."
  removeFile tempFile


promptAndContinue :: FilePath -> IO ()
promptAndContinue f = do
  hPutStrLn stderr
    $ unwords [ "Merged file", "\""++f++"\"", "already exists on disk."
              , "Do you want to continue? [y/n]"
              ]
  hSetBuffering stdin NoBuffering
  oldEcho <- hGetEcho stdin
  hSetEcho stdin False
  input <- getChar
  hSetEcho stdin oldEcho
  case input of
    'n' -> hPutStrLn stderr "Aborting..." >> exitFailure
    'y' -> hPutStrLn stderr "Continuing and letting ffmpeg overwrite the merged file."
    _   -> do
      putStrLn "Invalid input. Please enter 'y' or 'n'."
      promptAndContinue f

frameIndexInOut :: FilePath -> Bool -> Maybe [[ByteString]]
                -> IO ( FilePath, Float, Float, [[ByteString]] )
frameIndexInOut file isFirst csvPrevFile = do
  let frameIndexOutFile = makeFrameIndexExtension file

  oExist <- doesFileExist frameIndexOutFile
  kf <- newKnobFileOrOutput oExist frameIndexOutFile (Just $ frameIndexFromFile file)
  writeKnobToFile oExist frameIndexOutFile kf
  (_, tbLine, linesList) <- parseFrameIndexKnob kf

  let csvList = csvLines linesList
      lastFrameLine
        | not $ null l = Just l
        | otherwise = Nothing
        where
          l = last csvList
      pts = getPts lastFrameLine
  case lastFrameLine of
    Just _ -> pure ()
    Nothing -> exitWithErrorMsg "No last csv frameindex line retrieved."

  fileAbsolute <- makeAbsolute file

  let lastMd5PrevLine = fromMaybe (C.pack "")
                        $ last
                        ( fromMaybe [[C.pack ""]] csvPrevFile ) `Safe.atMay` 5
  let secOut = ptsToSec pts tbLine
  let secInMay
        | not isFirst = Just $ (`ptsToSec` tbLine) . getPts . childList lastMd5PrevLine $ csvList
        | otherwise = Nothing
      secIn = fromMaybe 0.0 secInMay
  when (secOut == 0.0) noHashMatchErr

  pure (fileAbsolute, secIn, secOut, csvList)

  where
    noHashMatchErr = exitWithErrorMsg $ unwords
        [ "No matching list containing hash."
        , "The second video may need fixup, is encoded differently or"
        , "is not the continuation of the first."
        ]

fpsProbePrint :: String -> String -> IO (Int, Int)
fpsProbePrint f1 f2 = do
  pf1 <- fpsProbe f1
  pf2 <- fpsProbe f2
  pure (readIntBS $ splitHead pf1, readIntBS $ splitHead pf2)
    where
      splitHead = head . C.split '/' . C.pack


