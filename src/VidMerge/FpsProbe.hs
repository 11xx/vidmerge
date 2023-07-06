<<<<<<< HEAD
module VidMerge.FpsProbe where
=======
module VidMerge.FpsProbe (fpsProbe) where
>>>>>>> parse-frameindex

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
<<<<<<< HEAD
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import System.IO ( hGetContents )
import Data.Char (isSpace)

fpsProbe :: String -> IO [Char]
=======
import System.IO ( hGetContents )
import Data.Char (isSpace)

fpsProbe :: String -> IO String
>>>>>>> parse-frameindex
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
