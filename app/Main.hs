module Main where

import VidMerge.FrameMd5 ( writeFrameIndexToFile )
import VidMerge.FpsProbe ( fpsProbe )

import Options ( optsParserInfo, execParser, Opts(Opts) )

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  frameIndexFromArgs opts
  fpsProbePrint opts

frameIndexFromArgs :: Opts -> IO ()
frameIndexFromArgs opts@(Opts f1 f2 _) = do
  writeFrameIndexToFile f1
  writeFrameIndexToFile f2

fpsProbePrint :: Opts -> IO ()
fpsProbePrint opts@(Opts f1 f2 _) = do
  pf1 <- fpsProbe f1
  putStrLn $ unwords [f1, "has fps", pf1]

  pf2 <- fpsProbe f2
  putStrLn $ unwords [f2, "has fps", pf2]
