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

data Color = Cyan | Blue 

instance showColor :: Show Color where
  show Cyan   = "#00FFFF"
  show Blue   = "#0000FF"
