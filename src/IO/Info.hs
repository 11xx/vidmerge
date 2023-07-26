module IO.Info ( infoMsg, infoMsgLn ) where

import System.IO ( hPutStr, hPutStrLn, stderr )
import IO.Colors ( reset, green )

infoMsgLn :: String -> IO ()
infoMsgLn msg = do
  infoMsgPrefix
  hPutStrLn stderr msg

infoMsg :: String -> IO ()
infoMsg msg = do
  infoMsgPrefix
  hPutStr stderr msg

infoMsgPrefix :: IO ()
infoMsgPrefix = do
  hPutStr stderr "["
  green
  hPutStr stderr "INFO"
  reset
  hPutStr stderr "] "
