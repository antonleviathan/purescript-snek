module Config where

import Prelude
import Math
import Data.Int (toNumber)

canvasHeight :: Number
canvasHeight = 600.0

canvasWidth :: Number
canvasWidth = 600.0

horizontalBlocks :: Int
horizontalBlocks = 40

verticalBlocks :: Int
verticalBlocks = 40

blockHeight :: Number
blockHeight = canvasHeight / toNumber verticalBlocks

blockWidth :: Number
blockWidth = canvasWidth / toNumber horizontalBlocks
