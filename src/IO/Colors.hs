module IO.Colors where

import System.Console.ANSI
import System.IO

reset :: IO ()
reset = hSetSGR stderr [Reset]

green :: IO ()
green = hSetSGR stderr [SetColor Foreground Vivid Green]

yellow :: IO ()
yellow = hSetSGR stderr [SetColor Foreground Vivid Yellow]

red :: IO ()
red = hSetSGR stderr [SetColor Foreground Vivid Yellow]

cyan :: IO ()
cyan = hSetSGR stderr [SetColor Foreground Vivid Cyan]
