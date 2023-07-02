module Main where

import VidMerge.FrameMd5
import VidMerge.FpsProbe ( fpsProbe )

import Options ( optsParserInfo, execParser, Opts(Opts) )

import qualified Data.ByteString as BS
import System.FilePath ( (<.>), dropExtensions )
import Data.Knob
import qualified Data.Knob as K

import System.IO

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  frameIndexFromArgs opts
  fpsProbePrint opts

frameIndexFromArgs :: Opts -> IO ()
frameIndexFromArgs (Opts f1 f2 _) = do
  -- file1
  kf1 <- frameIndexFromFile f1
  fhf1 <- newFileHandle kf1 (o1 <.> "tmp") WriteMode
  cf1 <- K.getContents kf1
  BS.writeFile o1 cf1
  putStrLn $ oMsg o1

  -- file2
  kf2 <- frameIndexFromFile f2
  fhf2 <- newFileHandle kf2 (o2 <.> "tmp") WriteMode
  cf2 <- K.getContents kf2
  BS.writeFile o2 cf2
  putStrLn $ oMsg o2

  where
    o1 = makeFrameIndexExtension f1
    o2 = makeFrameIndexExtension f2
    oMsg f = unwords ["Output has been", "written", "to", f]  -- testing unwords

  -- writeFrameIndexToFile f2

fpsProbePrint :: Opts -> IO ()
fpsProbePrint (Opts f1 f2 _) = do
  pf1 <- fpsProbe f1
  putStrLn $ unwords [f1, "has fps", pf1]

  pf2 <- fpsProbe f2
  putStrLn $ unwords [f2, "has fps", pf2]

makeFrameIndexExtension :: FilePath -> FilePath
makeFrameIndexExtension f = dropExtensions f <.> "frameindex.txt"
