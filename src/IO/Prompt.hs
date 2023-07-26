module IO.Prompt ( promptContinueMergedFileExists ) where

import System.Exit ( exitFailure )
import System.IO
    ( hPutStrLn,
      stderr,
      hGetEcho,
      hSetBuffering,
      hSetEcho,
      hPutStr,
      stdin,
      BufferMode(NoBuffering) )
import IO.Colors ( reset, cyan )

promptContinueMergedFileExists :: FilePath -> IO ()
promptContinueMergedFileExists f = do
  promptMsgLn $ unwords
    [ "Merged file", "\"" ++ f ++ "\"", "already exists on disk."
    , "Do you want to continue? [y/n]"
    ]
  hSetBuffering stdin NoBuffering
  oldEcho <- hGetEcho stdin
  hSetEcho stdin False
  input <- getChar
  hSetEcho stdin oldEcho
  case input of
    'n' -> hPutStrLn stderr "Aborting..." >> exitFailure
    'y' -> hPutStrLn stderr "Continuing and letting ffmpeg overwrite the merged file."
    _   -> do
      putStrLn "Invalid input. Please enter 'y' or 'n'."
      promptContinueMergedFileExists f

promptMsgPrefix :: IO ()
promptMsgPrefix = do
  hPutStr stderr "["
  cyan
  hPutStr stderr "PROMPT"
  reset
  hPutStr stderr "] "

promptMsgLn :: String -> IO ()
promptMsgLn msg = do
  promptMsgPrefix
  hPutStrLn stderr msg
