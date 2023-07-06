module VidMerge.FrameMd5 ( frameIndexFromFile
                         , makeFrameIndexExtension
                         , newKnobFileOrOutput
                         , writeKnobToFile ) where

import System.Process
    ( createProcess,
      proc,
      CreateProcess(std_out),
      StdStream(CreatePipe) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Knob ( newKnob, Knob )
import qualified Data.Knob as Knob
import System.FilePath ( dropExtensions, (<.>) )
import qualified Data.ByteString.Char8 as C
import IO.Error ( exitWithErrorMsg )

import System.Console.ANSI
    ( hSetSGR,
      Color(Green),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(Reset, SetColor) )

import System.IO ( hPutStrLn, stderr )


frameIndexFromFile :: String -> IO Knob
frameIndexFromFile f = do
  output <- readProcessOutput $ extractFrameMd5 f
  newKnob output

readProcessOutput :: CreateProcess -> IO ByteString
readProcessOutput pc = do
  (_, Just hout, _, _) <- createProcess pc { std_out = CreatePipe }
  BS.hGetContents hout

extractFrameMd5 :: String -> CreateProcess
extractFrameMd5 f =
  proc "ffmpeg"
       [ "-y"
       , "-loglevel", "fatal"
       , "-nostats"
       , "-hide_banner"
       , "-i", f
       , "-an"
       , "-f", "framemd5"
       , "-c", "copy"
       , "-"
       ]

makeFrameIndexExtension :: FilePath -> FilePath
makeFrameIndexExtension f = dropExtensions f <.> "frameindex.txt"

newKnobFileOrOutput :: Bool -> FilePath -> Maybe (IO Knob) -> IO Knob
newKnobFileOrOutput bool f maybeKnobFunc = do
  case (bool, maybeKnobFunc) of
    (True, _)          -> Knob.newKnob =<< C.readFile f
    (False, Just func) -> func
    (_, Nothing)       -> exitWithErrorMsg
                          "Missing output file or knob function."

writeKnobToFile :: Bool -> FilePath -> Knob -> IO ()
writeKnobToFile False f knob = do
  knobCont <- Knob.getContents knob
  BS.writeFile f knobCont
  green
  hPutStrLn stderr oMsg
  reset
    where
      oMsg = unwords [ "Output frameindex file has been"
                     , "written to", f]
      green = hSetSGR stderr [SetColor Foreground Vivid Green]
      reset = hSetSGR stderr [Reset]
writeKnobToFile _ _ _ = mempty
