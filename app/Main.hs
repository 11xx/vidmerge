module Main where

import VidMerge.FrameMd5 ( writeFrameIndexToFile )

import Options

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  frameIndexFromArgs opts

frameIndexFromArgs opts@(Opts file1 file2 _) = do
  writeFrameIndexToFile file1
  writeFrameIndexToFile file2
