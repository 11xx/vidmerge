module VidMerge.FpsProbe ( fpsProbe, fpsProbePrint ) where

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
import System.IO ( hGetContents )
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C
import VidMerge.ParseMd5

fpsProbe :: String -> IO String
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

fpsProbePrint :: String -> String -> IO (Int, Int)
fpsProbePrint f1 f2 = do
  pf1 <- fpsProbe f1
  pf2 <- fpsProbe f2
  pure (readIntBS $ splitHead pf1, readIntBS $ splitHead pf2)
    where
      splitHead = head . C.split '/' . C.pack
