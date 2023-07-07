module IO.Error (exitWithErrorMsg) where

import System.IO ( hPutStr, hPutStrLn, stderr )
import System.Exit ( exitFailure )
import System.Console.ANSI
    ( hSetSGR,
      Color(Red),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(Reset, SetColor) )


exitWithErrorMsg :: String -> IO ()
exitWithErrorMsg msg = do
  red
  hPutStr stderr "ERROR: "
  reset
  hPutStrLn stderr msg
  exitFailure
    where
      -- msg' = unwords msg
      red = hSetSGR stderr [SetColor Foreground Vivid Red]
      reset = hSetSGR stderr [Reset]
