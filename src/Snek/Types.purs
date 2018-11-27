module Snek.Types where

import Prelude
import Data.Enum
import Data.Ord
import Data.Maybe

type KeyCode = Int

data Direction = Up | Down | Left | Right
data Action = Move Direction

type Coordinate =
  { x :: Number
  , y :: Number
  }

type State =
  { current       :: Array Coordinate
  , direction     :: Int
  , currentRodent :: Coordinate
  , rodents       :: Array Coordinate
  }

data Color = Cyan | Blue | Orange | Yellow | Green | Purple | Red | White

instance showColor :: Show Color where
  show Cyan   = "#00FFFF"
  show Blue   = "#0000FF"
  show Orange = "#FFFF00"
  show Yellow = "#FFA500"
  show Green  = "#008000"
  show Purple = "#800080"
  show Red    = "#FF0000"
  show White  = "#FFFFFF"
