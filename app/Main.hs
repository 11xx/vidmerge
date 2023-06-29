module Main where

import VidMerge.FrameMd5 ( writeToFile )

import Options.Applicative ()

main :: IO ()
main = writeToFile "file.mp4" "output.txt"
