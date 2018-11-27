module Snek.Draw where

import Config
import Prelude
import Effect (Effect, foreachE)
import Effect.Random
import Effect.Ref

import Data.Maybe
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (range)
import Data.Int
import Graphics.Canvas

import Snek.Types

data LineDirection = Horizontal | Vertical

type Coordinate =
  { x :: Number
  , y :: Number
  }

coordinateToRectangle :: Coordinate -> Rectangle
coordinateToRectangle c =
  { x     : c.x
  , y     : c.y
  , width : blockWidth
  , height: blockHeight
  }

drawShapes :: Array Coordinate -> Context2D -> Effect Unit
drawShapes bc ctx = drawRects bc
  where
    drawRects r = foreachE r \i -> do
      let r = coordinateToRectangle i

      rect         ctx r
      setFillStyle ctx "#800080"
      fillRect     ctx r
      stroke       ctx

randomRodent :: State -> Context2D -> Effect Unit
randomRodent s ctx =
      do
        randX <- randomInt 1 1000
        randY <- randomInt 1 1000
        let randC = coordinateToRectangle { x: toNumber randX, y: toNumber randY }

        rect         ctx randC
        setFillStyle ctx "#300080"
        fillRect     ctx randC
        stroke       ctx

drawRodent :: State -> Context2D -> Effect Unit
drawRodent c ctx = drawRect c.currentRodent
  where
    drawRect coo = do
            let r = coordinateToRectangle coo

            rect         ctx r
            setFillStyle ctx "#300080"
            fillRect     ctx r
            stroke       ctx

drawGrid :: Int -> Int -> Context2D -> Effect Unit
drawGrid numHorizontal numVertical c2d = do
  drawLines numVertical   Vertical   c2d
  drawLines numHorizontal Horizontal c2d

drawLine :: Tuple Coordinate Coordinate -> Context2D -> Effect Unit
drawLine points c2d = do
  beginPath c2d
  moveTo    c2d from.x from.y
  lineTo    c2d to.x   to.y
  stroke    c2d
  where
    from = fst points
    to   = snd points

drawLines :: Int -> LineDirection -> Context2D -> Effect Unit
drawLines numL dir c2d = foreachE (range 0 numL) line
  where
    line = \i -> do drawLine (pos i dir) c2d
    startV i = { x: 0.0,                    y: (offset i canvasHeight) }
    endV   i = { x: canvasWidth,            y: (offset i canvasHeight) }
    startH i = { x: (offset i canvasWidth), y: 0.0 }
    endH   i = { x: (offset i canvasWidth), y: canvasHeight }
    offset i max        = (max  / (toNumber numL)) * (toNumber i)
    pos    i Horizontal = Tuple (startH i) (endH i)
    pos    i Vertical   = Tuple (startV i) (endV i)
