module Main where

import Config
import Prelude (Unit, bind, discard, identity, map, pure, unit, void, ($), (*), (+), (-), (==), (&&), (<>))
import Graphics.Canvas (clearRect, getCanvasElementById, getContext2D)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array
import Math ((%))
import Partial.Unsafe


import Effect (Effect)
import Effect.Ref (Ref, modify, new, read)
import Effect.Timer (IntervalId, setInterval)

import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.Event.Internal.Types (Event, EventTarget)

import Snek.Draw as Draw
import Snek.Types (Coordinate, State, KeyCode)

foreign import window :: EventTarget
foreign import keyCode :: Event -> Int

-- TODO: [ ] Write fn which checks if the 'leading' block is moving into the coordinates where the 'rodent'
--       is and make it respawn somewhere else
-- TODO: [ ] if x or y coordinate lies outside of the plane, restart game
-- TODO: [ ] if the snake touches itself, restart game
-- TODO: [ ] the snake can't go directly between (up and down) or (left and right) if longer than 1

eventL :: Ref State -> Effect EventListener
eventL ref = eventListener (keyPress ref)

keydownEvent :: EventType
keydownEvent = EventType "keydown"

keyPress :: Ref State -> Event -> Effect Unit
keyPress ref e = void $ modify move' ref
  where
    move' c =
      { current:   c.current
      , direction: keyCode e
      , rodent:    c.rodent
      }

data Direction = Left | Up | Right | Down
data Action = Move Direction

keyCodeToAction :: KeyCode -> Maybe Action
keyCodeToAction kc = case kc of
  37 -> Just (Move Left)
  38 -> Just (Move Up)
  39 -> Just (Move Right)
  40 -> Just (Move Down)
  _  -> Nothing

keyCodeToDir :: KeyCode -> Direction
keyCodeToDir kc = case kc of
  37 -> Left
  38 -> Up
  39 -> Right
  40 -> Down
  _  -> Down

moveBlocks :: Direction -> Coordinate -> Array Coordinate -> Array Coordinate
moveBlocks dir rodent arr =
  if (checkIfRodent rodent snakeHead) then
    pure newBlock <> arr
  else
    take ((length arr) ) (pure newBlock <> arr)
  where
    f c = case dir of
      Left  -> { x: c.x - blockWidth, y: c.y }
      Right -> { x: c.x + blockWidth, y: c.y }
      Down  -> { x: c.x,              y: c.y + blockHeight }
      Up    -> { x: c.x,              y: c.y - blockHeight }

    snakeHead = head arr
    newBlock = maybe { x: 2.0, y: 2.0 } (\x -> f x) snakeHead

checkIfRodent :: Coordinate -> Maybe Coordinate -> Boolean
checkIfRodent rodent snakeHead =
  case snakeHead of
    Nothing -> false
    Just snakeHead -> if snakeHead.x == rodent.x && snakeHead.y == rodent.y then true else false



initialRodent :: Coordinate
initialRodent = { x: blockHeight * 20.0, y: blockHeight * 20.0 }

initialState ::  State
initialState =
  { current:   [ { x: blockHeight * 3.0, y: blockHeight * 3.0 } ]
  , direction: 40
  , rodent:    initialRodent
  }



next :: State -> State
next s =
  { current:   moveBlocks (keyCodeToDir s.direction) s.rodent s.current
  , direction: s.direction
  , rodent:    s.rodent 
  }

main :: Partial => Effect Unit
main = void do
  Just canvas <- getCanvasElementById "snek-canvas"
  c2d         <- getContext2D canvas
  state       <- new initialState
  timer       <- new 0.0
  evF         <- eventL state

  addEventListener keydownEvent evF false window

  id <- setInterval 50 $ void do
    clearRect c2d { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight }
    Draw.drawGrid  horizontalBlocks verticalBlocks c2d
    s <- read state

    Draw.drawRodent s c2d
    Draw.drawShapes s.current c2d

    t <- read timer
    _ <- modify (if t % 1000.0 == 0.0 then next else identity) state
    modify ((+) 600.0) timer

  setInterval 600 $ void do
    st <- read state
    pure unit
