module VidMerge.FrameMd5 ( writeToFile ) where

import System.Process
    ( proc,
      createProcess,
      StdStream(CreatePipe),
      CreateProcess(std_out) )
import Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Lazy as BS
import System.IO ( withFile, IOMode(WriteMode) )

writeToFile :: [Char] -> FilePath -> IO ()
writeToFile f o = do
  output <- readProcessOutput $ extractFrameMd5 f
  withFile o WriteMode (`BS.hPutStr` output)
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

