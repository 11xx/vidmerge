module IO.Debug ( debugMsg, debugMsgLn, debugPrint ) where

import System.IO ( hPutStr, hPutStrLn, stderr, hPrint )
import IO.Colors ( reset, yellow )

debugMsgPrefix :: IO ()
debugMsgPrefix = do
  hPutStr stderr "["
  yellow
  hPutStr stderr "DEBUG"
  reset
  hPutStr stderr "] "

debugMsgLn :: String -> IO ()
debugMsgLn msg = do
  debugMsgPrefix
  hPutStrLn stderr msg

debugMsg :: String -> IO ()
debugMsg msg = do
  debugMsgPrefix
  hPutStr stderr msg


debugPrint :: Show a => a -> IO ()
debugPrint sym = do
  debugMsgPrefix
  hPrint stderr sym

