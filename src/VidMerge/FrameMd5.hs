module VidMerge.FrameMd5 ( frameIndexFromFile ) where

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Knob

frameIndexFromFile :: String -> IO Knob
frameIndexFromFile f = do
  output <- readProcessOutput $ extractFrameMd5 f
  newKnob output

readProcessOutput :: CreateProcess -> IO ByteString
readProcessOutput pc = do
  (_, Just hout, _, _) <- createProcess pc { std_out = CreatePipe }
  BS.hGetContents hout

extractFrameMd5 :: String -> CreateProcess
extractFrameMd5 f =
  proc "ffmpeg"
       [ "-y"
       , "-loglevel", "fatal"
       , "-nostats"
       , "-hide_banner"
       , "-i", f
       , "-an"
       , "-f", "framemd5"
       , "-c", "copy"
       , "-"
       ]
