module VidMerge.FrameMd5 ( writeToFile ) where

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import System.IO ( hClose, IOMode(WriteMode) )
import Data.Knob ( getContents, newFileHandle, newKnob )

writeToFile :: [Char] -> FilePath -> IO ()
writeToFile f o = do
  output <- readProcessOutput $ extractFrameMd5 f
  knob <- newKnob output
  h <- newFileHandle knob "knobFrameMd5Handle.tmp" WriteMode
  cont <- Data.Knob.getContents knob
  BS.writeFile o cont
  hClose h
  putStrLn $ "Output has been written to " ++ o


readProcessOutput :: CreateProcess -> IO ByteString
readProcessOutput pc = do
  (_, Just hout, _, _) <- createProcess pc { std_out = CreatePipe }
  BS.hGetContents hout

extractFrameMd5 :: String -> CreateProcess
extractFrameMd5 f =
  proc "ffmpeg"
       [ "-y"
       , "-loglevel"
       , "fatal"
       , "-nostats"
       , "-hide_banner"
       , "-i"
       , f
       , "-an"
       , "-f"
       , "framemd5"
       , "-c"
       , "copy"
       , "-"
       ]
