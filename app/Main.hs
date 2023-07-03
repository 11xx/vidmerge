module Main where

import VidMerge.FrameMd5 ( frameIndexFromFile )
import VidMerge.FpsProbe ( fpsProbe )

import Options ( optsParserInfo, execParser, Opts(Opts) )

import qualified Data.ByteString as BS
import System.FilePath ( (<.>), dropExtensions )
-- import Data.Knob ( newFileHandle )
import qualified Data.Knob as K

import qualified Data.ByteString.Char8 as C
import System.IO ( hPutStr, hPutStrLn, stderr )

import Parse.List (childList)

import System.Exit ( exitFailure )

import System.Console.ANSI
    ( hSetSGR,
      Color(Red),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(Reset, SetColor) )

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  frameIndexFromArgs opts
  fpsProbePrint opts

frameIndexFromArgs :: Opts -> IO ()
frameIndexFromArgs (Opts f1 f2 _) = do
  -- file1
  kf1 <- frameIndexFromFile f1
  -- fhf1 <- newFileHandle kf1 (o1 <.> "tmp") WriteMode
  cf1 <- K.getContents kf1  -- to write to file
  BS.writeFile o1 cf1
  putStrLn $ oMsg o1

  let linesListAll = C.lines cf1
      headerIndex = findHeaderIndex linesListAll
      -- headerLine = linesListAll !! headerIndex
      linesList = drop (headerIndex + 1) linesListAll

  let csvLines = map (map C.strip . C.split ',') linesList

  print $ last csvLines

  -- file2
  kf2 <- frameIndexFromFile f2
  -- fhf2 <- newFileHandle kf2 (o2 <.> "tmp") WriteMode
  cf2 <- K.getContents kf2
  BS.writeFile o2 cf2
  putStrLn $ oMsg o2

  let linesListAll2 = C.lines cf2
      headerIndex2 = findHeaderIndex linesListAll2
      -- headerLine2 = linesListAll2 !! headerIndex2
      linesList2 = drop (headerIndex2 + 1) linesListAll2

  let csvLines2 = map (map C.strip . C.split ',') linesList2
      getChildList = childList (last csvLines !! 5) csvLines2  -- 6th item

  case getChildList of
    Just cl -> print cl
    Nothing -> do
      red
      hPutStr stderr "ERROR: "
      reset
      hPutStrLn stderr msg
      exitFailure
        where
          msg = unwords [ "No matching list containing hash."
                        , "Second video may need fixup or"
                        , "is not the continuation of the first."
                        ]
          red = hSetSGR stderr [SetColor Foreground Vivid Red]
          reset = hSetSGR stderr [Reset]

  where
    o1 = makeFrameIndexExtension f1
    o2 = makeFrameIndexExtension f2
    oMsg f = unwords ["Output has been", "written", "to", f]  -- testing unwords

findHeaderIndex :: [C.ByteString] -> Int
findHeaderIndex = go 0
  where
    go index (line:rest)
      | C.isPrefixOf (C.pack "#stream#, dts,        pts, duration,     size, hash") line = index
      | otherwise = go (index + 1) rest
    go _ [] = error "Header not found in CSV frameindex."

fpsProbePrint :: Opts -> IO ()
fpsProbePrint (Opts f1 f2 _) = do
  pf1 <- fpsProbe f1
  putStrLn $ unwords [f1, "has fps", pf1]

  pf2 <- fpsProbe f2
  putStrLn $ unwords [f2, "has fps", pf2]

makeFrameIndexExtension :: FilePath -> FilePath
makeFrameIndexExtension f = dropExtensions f <.> "frameindex.txt"
