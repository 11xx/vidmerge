module Main where

import VidMerge.FrameMd5 ( writeToFile )

import Options

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  run opts

run opts@(Opts f o _) = do
  writeToFile f o
