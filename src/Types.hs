{- Types
By Gregory W. Schwartz

Collects the types used in the program
-}

module Types where

-- Algebraic
data Rotation = Normal | BottomUp | LeftUp | RightUp

-- Simple
type Command   = String
type Device    = String
type Matrix    = String
type TouchFlag = String
