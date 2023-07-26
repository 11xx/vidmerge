module IO.Error (exitWithErrorMsg, exitWithErrorMsgNoHashMatch) where

import System.IO ( hPutStr, hPutStrLn, stderr )
import System.Exit ( exitFailure )
import IO.Colors ( reset, red )

errorMsgPrefix :: IO ()
errorMsgPrefix = do
  hPutStr stderr "["
  red
  hPutStr stderr "ERROR: "
  reset
  hPutStr stderr "] "

exitWithErrorMsg :: String -> IO ()
exitWithErrorMsg msg = do
  errorMsgPrefix
  hPutStrLn stderr msg
  exitFailure

exitWithErrorMsgNoHashMatch :: IO ()
exitWithErrorMsgNoHashMatch = exitWithErrorMsg $ unwords
  [ "No matching list containing hash."
  , "The second video may need fixup, is encoded differently or"
  , "is not the continuation of the first."
  ]
