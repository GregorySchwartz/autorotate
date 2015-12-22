{- autorotate
By Gregory W. Schwartz

Autorotate the screen, depends on iio-sensor-proxy containing
monitor-sensor. Gets the output of monitor-sensor through stderr.
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
    devList <- readProcess "xinput" ["--list", "--name-only"] []
    (_, _, Just hstderr, _) <-
        createProcess (proc "monitor-sensor" []){ std_err = CreatePipe }

    let dev        = findIn "touchscreen" . lines $ devList
        touch      = findIn "touchpad" . lines $ devList
        rotatePipe = rotate dev touch . sensorToRotation

    go hstderr rotatePipe
  where
    go hstderr rotatePipe = do
        contents <- T.hGetLine hstderr

        rotatePipe contents
        go hstderr rotatePipe
