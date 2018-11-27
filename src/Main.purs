module Main where

import Config
import Prelude (Unit, bind, discard, identity, map, pure, unit, void, ($), (*), (+), (-), (==), (&&), (<>))
import Graphics.Canvas (clearRect, getCanvasElementById, getContext2D)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array (head, tail, take, length)
import Math ((%))
import Partial.Unsafe
import Data.List.Lazy.Types


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
      { current:       c.current
      , direction:     keyCode e
      , currentRodent: c.currentRodent
      , rodents:       c.rodents
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

initialRodents :: Array Coordinate
initialRodents = [{ x: 120.0, y: 15.0 },{ x: 120.0, y: 30.0 },{ x: 120.0, y: 45.0 },{ x: 120.0, y: 60.0 },{ x: 120.0, y: 75.0 },{ x: 120.0, y: 90.0 },{ x: 120.0, y: 105.0 },{ x: 120.0, y: 120.0 },{ x: 120.0, y: 135.0 },{ x: 120.0, y: 150.0 },{ x: 120.0, y: 165.0 },{ x: 120.0, y: 180.0 },{ x: 120.0, y: 195.0 },{ x: 120.0, y: 210.0 },{ x: 135.0, y: 15.0 },{ x: 135.0, y: 30.0 },{ x: 135.0, y: 45.0 },{ x: 135.0, y: 60.0 },{ x: 135.0, y: 75.0 },{ x: 135.0, y: 90.0 },{ x: 135.0, y: 105.0 },{ x: 135.0, y: 120.0 },{ x: 135.0, y: 135.0 },{ x: 135.0, y: 150.0 },{ x: 135.0, y: 165.0 },{ x: 135.0, y: 180.0 },{ x: 135.0, y: 195.0 },{ x: 135.0, y: 210.0 },{ x: 150.0, y: 15.0 },{ x: 150.0, y: 30.0 },{ x: 150.0, y: 45.0 },{ x: 150.0, y: 60.0 },{ x: 150.0, y: 75.0 },{ x: 150.0, y: 90.0 },{ x: 150.0, y: 105.0 },{ x: 150.0, y: 120.0 },{ x: 150.0, y: 135.0 },{ x: 150.0, y: 150.0 },{ x: 150.0, y: 165.0 },{ x: 150.0, y: 180.0 },{ x: 150.0, y: 195.0 },{ x: 150.0, y: 210.0 },{ x: 165.0, y: 15.0 },{ x: 165.0, y: 30.0 },{ x: 165.0, y: 45.0 },{ x: 165.0, y: 60.0 },{ x: 165.0, y: 75.0 },{ x: 165.0, y: 90.0 },{ x: 165.0, y: 105.0 },{ x: 165.0, y: 120.0 },{ x: 165.0, y: 135.0 },{ x: 165.0, y: 150.0 },{ x: 165.0, y: 165.0 },{ x: 165.0, y: 180.0 },{ x: 165.0, y: 195.0 },{ x: 165.0, y: 210.0 },{ x: 180.0, y: 15.0 },{ x: 180.0, y: 30.0 },{ x: 180.0, y: 45.0 },{ x: 180.0, y: 60.0 },{ x: 180.0, y: 75.0 },{ x: 180.0, y: 90.0 },{ x: 180.0, y: 105.0 },{ x: 180.0, y: 120.0 },{ x: 180.0, y: 135.0 },{ x: 180.0, y: 150.0 },{ x: 180.0, y: 165.0 },{ x: 180.0, y: 180.0 },{ x: 180.0, y: 195.0 },{ x: 180.0, y: 210.0 },{ x: 195.0, y: 15.0 },{ x: 195.0, y: 30.0 },{ x: 195.0, y: 45.0 },{ x: 195.0, y: 60.0 },{ x: 195.0, y: 75.0 },{ x: 195.0, y: 90.0 },{ x: 195.0, y: 105.0 },{ x: 195.0, y: 120.0 },{ x: 195.0, y: 135.0 },{ x: 195.0, y: 150.0 },{ x: 195.0, y: 165.0 },{ x: 195.0, y: 180.0 },{ x: 195.0, y: 195.0 },{ x: 195.0, y: 210.0 },{ x: 210.0, y: 15.0 },{ x: 210.0, y: 30.0 },{ x: 210.0, y: 45.0 },{ x: 210.0, y: 60.0 },{ x: 210.0, y: 75.0 },{ x: 210.0, y: 90.0 },{ x: 210.0, y: 105.0 },{ x: 210.0, y: 120.0 },{ x: 210.0, y: 135.0 },{ x: 210.0, y: 150.0 },{ x: 210.0, y: 165.0 },{ x: 210.0, y: 180.0 },{ x: 210.0, y: 195.0 },{ x: 210.0, y: 210.0 },{ x: 15.0, y: 15.0 },{ x: 15.0, y: 30.0 },{ x: 15.0, y: 45.0 },{ x: 15.0, y: 60.0 },{ x: 15.0, y: 75.0 },{ x: 15.0, y: 90.0 },{ x: 15.0, y: 105.0 },{ x: 15.0, y: 120.0 },{ x: 15.0, y: 135.0 },{ x: 15.0, y: 150.0 },{ x: 15.0, y: 165.0 },{ x: 15.0, y: 180.0 },{ x: 15.0, y: 195.0 },{ x: 15.0, y: 210.0 },{ x: 30.0, y: 15.0 },{ x: 30.0, y: 30.0 },{ x: 30.0, y: 45.0 },{ x: 30.0, y: 60.0 },{ x: 30.0, y: 75.0 },{ x: 30.0, y: 90.0 },{ x: 30.0, y: 105.0 },{ x: 30.0, y: 120.0 },{ x: 30.0, y: 135.0 },{ x: 30.0, y: 150.0 },{ x: 30.0, y: 165.0 },{ x: 30.0, y: 180.0 },{ x: 30.0, y: 195.0 },{ x: 30.0, y: 210.0 },{ x: 45.0, y: 15.0 },{ x: 45.0, y: 30.0 },{ x: 45.0, y: 45.0 },{ x: 45.0, y: 60.0 },{ x: 45.0, y: 75.0 },{ x: 45.0, y: 90.0 },{ x: 45.0, y: 105.0 },{ x: 45.0, y: 120.0 },{ x: 45.0, y: 135.0 },{ x: 45.0, y: 150.0 },{ x: 45.0, y: 165.0 },{ x: 45.0, y: 180.0 },{ x: 45.0, y: 195.0 },{ x: 45.0, y: 210.0 },{ x: 60.0, y: 15.0 },{ x: 60.0, y: 30.0 },{ x: 60.0, y: 45.0 },{ x: 60.0, y: 60.0 },{ x: 60.0, y: 75.0 },{ x: 60.0, y: 90.0 },{ x: 60.0, y: 105.0 },{ x: 60.0, y: 120.0 },{ x: 60.0, y: 135.0 },{ x: 60.0, y: 150.0 },{ x: 60.0, y: 165.0 },{ x: 60.0, y: 180.0 },{ x: 60.0, y: 195.0 },{ x: 60.0, y: 210.0 },{ x: 75.0, y: 15.0 },{ x: 75.0, y: 30.0 },{ x: 75.0, y: 45.0 },{ x: 75.0, y: 60.0 },{ x: 75.0, y: 75.0 },{ x: 75.0, y: 90.0 },{ x: 75.0, y: 105.0 },{ x: 75.0, y: 120.0 },{ x: 75.0, y: 135.0 },{ x: 75.0, y: 150.0 },{ x: 75.0, y: 165.0 },{ x: 75.0, y: 180.0 },{ x: 75.0, y: 195.0 },{ x: 75.0, y: 210.0 },{ x: 90.0, y: 15.0 },{ x: 90.0, y: 30.0 },{ x: 90.0, y: 45.0 },{ x: 90.0, y: 60.0 },{ x: 90.0, y: 75.0 },{ x: 90.0, y: 90.0 },{ x: 90.0, y: 105.0 },{ x: 90.0, y: 120.0 },{ x: 90.0, y: 135.0 },{ x: 90.0, y: 150.0 },{ x: 90.0, y: 165.0 },{ x: 90.0, y: 180.0 },{ x: 90.0, y: 195.0 },{ x: 90.0, y: 210.0 },{ x: 105.0, y: 15.0 },{ x: 105.0, y: 30.0 },{ x: 105.0, y: 45.0 },{ x: 105.0, y: 60.0 },{ x: 105.0, y: 75.0 },{ x: 105.0, y: 90.0 },{ x: 105.0, y: 105.0 },{ x: 105.0, y: 120.0 },{ x: 105.0, y: 135.0 },{ x: 105.0, y: 150.0 },{ x: 105.0, y: 165.0 },{ x: 105.0, y: 180.0 },{ x: 105.0, y: 195.0 },{ x: 105.0, y: 210.0 }]


initialRodent :: Coordinate
initialRodent = { x: blockHeight * 20.0, y: blockHeight * 20.0 }

initialState ::  State
initialState =
  { current:   [ { x: blockHeight * 3.0, y: blockHeight * 3.0 } ]
  , direction: 40
  , currentRodent: initialRodent
  , rodents:    initialRodents
  }

newRodent :: State -> Array Coordinate -> Coordinate
newRodent s arr =
  case head arr of
    Nothing -> s.currentRodent
    Just h -> h

shuffleRodents :: State -> Array Coordinate
shuffleRodents s =
  case tail s.rodents of
    Nothing -> s.rodents
    Just t -> t <> pure s.currentRodent

next :: State -> State
next s =
  { current:       moveBlocks (keyCodeToDir s.direction) s.currentRodent s.current
  , direction:     s.direction
  , currentRodent: if checkIfRodent s.currentRodent (head s.current) then newRodent s s.rodents else s.currentRodent
  , rodents:       if checkIfRodent s.currentRodent (head s.current) then shuffleRodents s else s.rodents
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
