{-# LANGUAGE DeriveGeneric #-}

module Types (
    Mode (..),
    Modes (..)
) where

import GHC.Generics

data Mode = Mode {
    isType :: String,
    isTflService :: Bool,
    isFarePaying :: Bool,
    isScheduledService :: Bool,
    modeName :: String
} deriving (Show, Generic)

type Modes = [Mode]