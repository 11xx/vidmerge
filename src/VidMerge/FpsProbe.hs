module VidMerge.FpsProbe (fpsProbe) where

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
import System.IO ( hGetContents )
import Data.Char (isSpace)

fpsProbe :: String -> IO [Char]
fpsProbe f = do
  (_, Just out, _, _) <- createProcess $ (fpsProbeCmd f) { std_out = CreatePipe }
  hout <- hGetContents out

  pure $ rstrip hout

fpsProbeCmd :: String -> CreateProcess
fpsProbeCmd f =
  proc "ffprobe"
       [ "-v", "0"
       , "-of", "csv=p=0"
       , "-select_streams"
       , "v:0"
       , "-show_entries"
       , "stream=r_frame_rate"
       , f
       ]

-- remove \n at the end
rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse
