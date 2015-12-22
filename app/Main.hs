{- autorotate
By Gregory W. Schwartz

Autorotate the screen, depends on iio-sensor-proxy containing
monitor-sensor. To use, pipe monitor-sensor to stdout to autorotate:
monitor-sensor 2>&1 >/dev/null | autorotate
-}

module Main where

-- Standard
import Data.Char
import Data.List
import System.Process
import System.IO

-- Cabal
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Local
import Types
import Rotate

-- | Find subtext in a list of text
findIn :: String -> [String] -> String
findIn s ss = case filter (isInfixOf s . map toLower) ss of
                  []   -> error (s ++ " not found")
                  x:xs -> "\"" ++ x ++ "\""

main :: IO ()
main = do
    client  <- connectSession
    devList <- readProcess "xinput" ["--list", "--name-only"] []

    let dev        = findIn "touchscreen" . lines $ devList
        touch      = findIn "touchpad" . lines $ devList
        rotatePipe = rotate dev touch . sensorToRotation

    go rotatePipe
  where
    go rotatePipe = do
        contents <- T.hGetLine stderr

        rotatePipe contents
        go rotatePipe
