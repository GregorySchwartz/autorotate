{- Rotate
By Gregory W. Schwartz

Collects the functions pertaining to checking orientation and rotating the screen
-}

{-# LANGUAGE OverloadedStrings #-}

module Rotate
    ( rotate
    , sensorToRotation
    ) where

-- Standard
import System.Process

-- Cabal
import qualified Data.Text as T

-- Local
import Types

-- | Command to rotate screen
toCommand :: Device -> Matrix -> Command
toCommand dev matrix = "xinput --set-prop "
                    ++ dev
                    ++ " \"Coordinate Transformation Matrix\" "
                    ++ matrix

-- | Rotate the screen based on the incoming message
rotate :: Device -> Device -> Rotation -> IO ()
rotate dev touch Normal   = do
    _ <- system "xrandr -o normal"
    _ <- system . toCommand dev $ "1 0 0 0 1 0 0 0 1"
    _ <- system $ "xinput enable " ++ touch
    return ()
rotate dev touch BottomUp = do
    _ <- system "xrandr -o inverted"
    _ <- system . toCommand dev $ "-1 0 1 0 -1 1 0 0 1"
    _ <- system $ "xinput disable " ++ touch
    return ()
rotate dev touch LeftUp   = do
    _ <- system "xrandr -o left"
    _ <- system . toCommand dev $ "0 -1 1 1 0 0 0 0 1"
    _ <- system $ "xinput disable " ++ touch
    return ()
rotate dev touch RightUp  = do
    _ <- system "xrandr -o right"
    _ <- system . toCommand dev $ "0 1 0 -1 0 1 0 0 1"
    _ <- system $ "xinput disable " ++ touch
    return ()

-- | Convert iio-sensor-proxy monitor-sensor output to a type
sensorToRotation :: T.Text -> Rotation
sensorToRotation x = case snd . T.breakOnEnd ": " $ x of
                       "normal"    -> Normal
                       "bottom-up" -> BottomUp
                       "left-up"   -> LeftUp
                       "right-up"  -> RightUp
