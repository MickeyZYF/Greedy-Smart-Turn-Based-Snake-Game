module Main(main, GameState, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import System.IO.Unsafe
import Debug.Trace

grid, width, height, speed, gridHalf:: Int
grid = 20
gridHalf = 10
width = 40
height = 40
offset = 0
speed = 50

maxX, minX, maxY, minY :: Int
maxY = grid * (div height 2)
minY = grid * (-(div height 2))
maxX = grid * (div width 2)
minX = grid * (-(div width 2))

gameUI :: Display
gameUI = InWindow "Haskell Huddle" (width * grid, height * grid) (offset, offset)

background, wallColor :: Color
background = black
wallColor = white

foodColor, sArrowColor, gArrowColor :: Color
foodColor = yellow
sArrowColor = blue
gArrowColor = red


data GameState = Game 
   { foodLoc      :: (Int, Int)
   , snakeLoc     :: [(Int, Int)]
   , snakeDir     :: [Char] 
   , greedyArrow  :: [Char]
   , smartArrow   :: [Char]
   , menu         :: Int
   , score        :: Int
   , snakeColor   :: Color
   , poisonLoc1    :: (Int, Int)
   , poisonLoc2    :: (Int, Int)
   , poisonLoc3    :: (Int, Int)
   , multiplier   :: Int
   } deriving Show


initialState :: GameState
initialState = Game
  { foodLoc = (-160, -160) -- starting point so it doesn't collide with obstacles
  , snakeLoc = [(0, 0)] -- centre of screen
  , snakeDir = "N"
  , greedyArrow = "W" -- tip for best route that doesn't care about obstacles
  , smartArrow = "W" -- tip for best route that cares about obstacles
  , menu = 1 --main menu
  , score = 0
  , snakeColor = green
  , poisonLoc1 = (160, 160) --three obstacles
  , poisonLoc2 = (-160, 160)
  , poisonLoc3 = (160, -160)
  , multiplier = 5 -- score multiplier
  }

-- convert game state to picture
render :: GameState -> Picture
render game 
-- startscreen
 | (menu game) == 1 = pictures [startScreenImage1, startScreenImage2, startScreenImage3, startScreenText, authors] 
-- end screen
 | (menu game) == 0 = pictures [endScreenImage, endScreenText]
-- the game itself
 | otherwise = pictures [food, snake, gArrow, sArrow, wallW, wallN, wallE, wallS, scoreBoard, poison1, poison2, poison3]
 where

-- assets for start screen
      startScreenImage1 = uncurry translate (-850,100) $ (color blue $ (Scale 1 1 (Text "Two")))
      startScreenImage2 = uncurry translate (-525,100) $ (color red $ (Scale 1 1 (Text "Headed")))
      startScreenImage3 = uncurry translate (0,100) $ (color green $ (Scale 1 1 (Text "Snake Game!")))
      startScreenText = uncurry translate (-400,0) $ color white $ (Scale 0.2 0.2 (Text "Press SPACE to start the game or ESC anytime to quit!"))

-- assets for end screen
      authors = uncurry translate (600,-500) $ color white $ (Scale 0.1 0.1 (Text "Presented by Mickey, David and Truman"))
      endScreenImage = uncurry translate (-350,100) $ color red $ (Scale 1 1 (Text "Game Over!"))
      endScreenText = uncurry translate (-250,0) $ color white $ (Scale 0.2 0.2 (Text "Press ESC to quit or SPACE to play again!"))

--food
      food = uncurry translate loc $ color foodColor $ rectangleSolid 10 10
      loc = (fromIntegral (fst (foodLoc game)), fromIntegral (snd (foodLoc game)))

--poison
      poison1 = uncurry translate ploc1 $ color white $ rectangleSolid 20 20
      ploc1 = (fromIntegral (fst (poisonLoc1 game)), fromIntegral (snd (poisonLoc1 game)))
      poison2 = uncurry translate ploc2 $ color white $ rectangleSolid 20 20
      ploc2 = (fromIntegral (fst (poisonLoc2 game)), fromIntegral (snd (poisonLoc2 game)))
      poison3 = uncurry translate ploc3 $ color white $ rectangleSolid 20 20
      ploc3 = (fromIntegral (fst (poisonLoc3 game)), fromIntegral (snd (poisonLoc3 game)))

--snake
      snake = drawHelper ([ (fromIntegral(x), fromIntegral(y)) | (x, y) <- snakeLoc game])
      drawHelper lst = pictures (drawPart lst (snakeColor game))
              where
                    drawPart [ ] _ = [ ]
                    drawPart (h:t) c = (renderPart h c):(drawPart t c)
                    renderPart (x, y) c = color c $ translate x y (rectangleSolid (fromIntegral grid) (fromIntegral grid))

--wall
      wallW = color wallColor $ translate (fromIntegral maxX) 0 (rectangleSolid (fromIntegral grid) wallHeight)
      wallE = color wallColor $ translate (fromIntegral minX) 0 (rectangleSolid (fromIntegral grid) wallHeight)
      wallN = color wallColor $ translate 0 (fromIntegral maxY) (rectangleSolid wallWidth (fromIntegral grid))
      wallS = color wallColor $ translate 0 (fromIntegral minY) (rectangleSolid wallWidth (fromIntegral grid))
      wallWidth = fromIntegral (width * grid)
      wallHeight = fromIntegral (height * grid)

--Score
      scoreBoard = uncurry translate scoreLoc $ color white $ Scale 1 1 (Text (show (score game)))
      scoreLoc = (fromIntegral (((div (width * grid) 2)) + 20), fromIntegral ((div (height*grid - maxY)  2)) + 50)

--greedyarrow
      gArrow
          | (greedyArrowDirection game) == (smartArrowDirection game) = arrowFunction violet (greedyArrowDirection game)
          | otherwise = arrowFunction gArrowColor (greedyArrowDirection game)
            where
              arrowFunction c dir
                | dir == "N/A" = uncurry translate (x, y) $ color c $ (Scale 1 1 (Text ("")))
                | dir == "N" = uncurry translate (x, y + zHalf) $ color c $ rectangleSolid zHalf zHalf
                | dir == "S" = uncurry translate (x, y - zHalf) $ color c $ rectangleSolid zHalf zHalf
                | dir == "E" = uncurry translate (x + zHalf, y) $ color c $ rectangleSolid zHalf zHalf
                | dir == "W" = uncurry translate (x - zHalf, y) $ color c $ rectangleSolid zHalf zHalf
                | otherwise = uncurry translate (x, y) $ color c $ (Scale 1 1 (Text ("")))
                    where
                      x = fromIntegral (fst (head (snakeLoc game)))
                      y = fromIntegral (snd (head (snakeLoc game)))
                      z = fromIntegral grid
                      zHalf = fromIntegral gridHalf

--smartarrow
      sArrow
          | (greedyArrowDirection game) == (smartArrowDirection game) = arrowFunction2 violet (smartArrowDirection game)
          | otherwise = arrowFunction2 sArrowColor (smartArrowDirection game)
            where
              arrowFunction2 c dir
                | dir == "N/A" = uncurry translate (x + zHalf, y) $ color c $ (Scale 1 1 (Text ("")))
                | dir == "N" = uncurry translate (x, y + zHalf) $ color c $ rectangleSolid zHalf zHalf
                | dir == "S" = uncurry translate (x, y - zHalf) $ color c $ rectangleSolid zHalf zHalf
                | dir == "E" = uncurry translate (x + zHalf, y) $ color c $ rectangleSolid zHalf zHalf
                | dir == "W" = uncurry translate (x - zHalf, y) $ color c $ rectangleSolid zHalf zHalf
                | otherwise = uncurry translate (x, y) $ color c $ (Scale 1 1 (Text ("")))
                    where
                      x = fromIntegral (fst (head (snakeLoc game)))
                      y = fromIntegral (snd (head (snakeLoc game)))
                      z = fromIntegral grid
                      zHalf = fromIntegral gridHalf

--movement function for snake. called by handle keys
update :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
update _ _ [ ] = [ ]
update x y lst = (x, y):(init lst)

--grow function for snake when it eats. called by update
update2 :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
update2 _ _ [ ] = [ ]
update2 x y (h:t) = (x, y):(update2 (fst h) (snd h) t)


--determines the next position of the snake for both the movement and grow function above.
temp :: String -> Int -> [(Int, Int)] -> [(Int, Int)]
temp z d loc
 | z == "N" && d == 1 = update x (y + gridHalf) loc
 | z == "S" && d == 1 = update x (y - gridHalf) loc
 | z == "E" && d == 1 = update (x + gridHalf) y loc
 | z == "W" && d == 1 = update (x - gridHalf) y loc
 | z == "N" && d == 2 = update2 x (y + grid) loc
 | z == "S" && d == 2 = update2 x (y - grid) loc
 | z == "E" && d == 2 = update2 (x + grid) y loc
 | z == "W" && d == 2 = update2 (x - grid) y loc
   where 
     x = fromIntegral (fst (head loc))
     y = fromIntegral (snd  (head loc))

--respond to key events. char keys
handleKeys (EventKey (Char key) _ _ _) game 
 | key == 'w' && (snakeDir game) /= "S"  = game {snakeLoc = temp "N" 1 (snakeLoc game), snakeDir = "N", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "N" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "N" 1 (snakeLoc game)})}
 | key == 's' && (snakeDir game) /= "N"  = game {snakeLoc = temp "S" 1 (snakeLoc game), snakeDir = "S", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "S" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "S" 1 (snakeLoc game)})}
 | key == 'd' && (snakeDir game) /= "W"  = game {snakeLoc = temp "E" 1 (snakeLoc game), snakeDir = "E", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "E" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "E" 1 (snakeLoc game)})}
 | key == 'a' && (snakeDir game) /= "E"  = game {snakeLoc = temp "W" 1 (snakeLoc game), snakeDir = "W", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "W" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "W" 1 (snakeLoc game)})}
 | otherwise  = game
     

--respond to special keys.
handleKeys (EventKey (SpecialKey key) _ _ _) game
 | key == KeyUp && (snakeDir game) /= "S" = game {snakeLoc = temp "N" 1 (snakeLoc game), snakeDir = "N", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "N" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "N" 1 (snakeLoc game)})}
 | key == KeyDown && (snakeDir game) /= "N" = game {snakeLoc = temp "S" 1 (snakeLoc game), snakeDir = "S", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "S" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "S" 1 (snakeLoc game)})}
 | key == KeyRight && (snakeDir game) /= "W" = game {snakeLoc = temp "E" 1 (snakeLoc game), snakeDir = "E", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "E" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "E" 1 (snakeLoc game)})}
 | key == KeyLeft && (snakeDir game) /= "E" = game {snakeLoc = temp "W" 1 (snakeLoc game), snakeDir = "W", greedyArrow = greedyArrowDirection (game {snakeLoc = temp "W" 1 (snakeLoc game)}), smartArrow = smartArrowDirection (game {snakeLoc = temp "W" 1 (snakeLoc game)})}
 | (menu game) == 1 && key == KeySpace = initialState { menu = 2 }
 | (menu game) == 0 && key == KeySpace = game { menu = 1 }
 | key == KeyEsc = game { menu = 1 }
 | otherwise = game

handleKeys _ game = game


--boolean for if the snake overlaps with itself
checkIntersect :: (Int, Int) -> [(Int, Int)] -> Bool
checkIntersect snakeHead [] = False
checkIntersect snakeHead (h:t)
   | (snakeHead == h) = True
   | otherwise = checkIntersect snakeHead t

--boolean for if snake overlaps with the wall
checkBoundary :: (Int, Int) -> Bool
checkBoundary (x, y)
   | (x < minX + 1) || (x > maxX - 1) || (y < minY + 1) || (y > maxY - 1) = True
   | otherwise = False

--boolean for if the snake overlaps with the obstacles
checkPoison :: (Int, Int) -> Bool
checkPoison (x, y)
    | (x == 160) && (y == 160) = True
    | (x == -160) && (y == 160) = True
    | (x == 160) && (y == -160) = True
    | otherwise = False

-- Given snakeLoc, checks if the snake should die
snakeDeath :: [(Int, Int)] -> Bool
snakeDeath (h:t) = (checkIntersect h t) || (checkBoundary h) || (checkPoison h)


-- Given a direction, and snakeLoc, gives next snakeLoc after going in that direction
moveSnake :: [Char] -> [(Int, Int)] -> [(Int, Int)]
moveSnake direction snake
    | (direction == "N") = moveNorth
    | (direction == "S") = moveSouth
    | (direction == "E") = moveEast
    | (direction == "W") = moveWest
    where
        x = fromIntegral (fst (head snake))
        y = fromIntegral (snd (head snake))
        moveNorth = update x (y + grid) snake
        moveSouth = update x (y - grid) snake
        moveEast  = update (x + grid) y snake
        moveWest  = update (x - grid) y snake

-- Given a direction, snakeLoc, calculate how many valid steps can be taken in that direction
snakePrediction :: [Char] -> [(Int, Int)] -> Int
snakePrediction direction snake
    | (checkDirection) = grid + snakePrediction direction moveDirection
    | otherwise = 0
    where
        moveDirection = moveSnake direction snake
        checkDirection = not (snakeDeath moveDirection)

-- Given a direction and game state, calculates the greediest direction the snake can go
greedyStep :: [Char] -> GameState -> [Char]
greedyStep foodDir state
    -- If the food is only in one direction from the food (N, E, S, W):
    -- 1. If the food is only in one direction from the snake, we try to go in that direction if it is valid
    -- 2. If there isn't a direct path to food, try another valid direction, prioritizing the directions that isn't
    -- the opposite of that of the food
    -- Food is North
    | (foodDir == "N") && (curDir /= "S") && (checkDirection "N") = "N"
    | (foodDir == "N") && (curDir /= "W") && (checkDirection "E") = "E"
    | (foodDir == "N") && (curDir /= "E") && (checkDirection "W") = "W"
    | (foodDir == "N") && (curDir /= "N") && (checkDirection "S") = "S"
    -- Food is East
    | (foodDir == "E") && (curDir /= "W") && (checkDirection "E") = "E"
    | (foodDir == "E") && (curDir /= "S") && (checkDirection "N") = "N"
    | (foodDir == "E") && (curDir /= "N") && (checkDirection "S") = "S"
    | (foodDir == "E") && (curDir /= "E") && (checkDirection "W") = "W"
    -- Food is South
    | (foodDir == "S") && (curDir /= "N") && (checkDirection "S") = "S"
    | (foodDir == "S") && (curDir /= "W") && (checkDirection "E") = "E"
    | (foodDir == "S") && (curDir /= "E") && (checkDirection "W") = "W"
    | (foodDir == "S") && (curDir /= "S") && (checkDirection "N") = "N"
    -- Food is West
    | (foodDir == "W") && (curDir /= "E") && (checkDirection "W") = "W"
    | (foodDir == "W") && (curDir /= "S") && (checkDirection "N") = "N"
    | (foodDir == "W") && (curDir /= "N") && (checkDirection "S") = "S"
    | (foodDir == "W") && (curDir /= "W") && (checkDirection "E") = "E"
    -- If the food is in two direction from the food (NE, SE, SW, NW):
    -- 1. If the snake is already going in one of the directions of the food, attempt to stay on it
    -- 2. If the snake can move in one of the directions of the food, attempt to turn to it
    -- 3. If the snake can't go in the directions of the food, try another another valid direction
    -- Food is North-East
    | (foodDir == "NE") && (curDir == "N") && (checkDirection "N") = "N"
    | (foodDir == "NE") && (curDir == "E") && (checkDirection "E") = "E"
    | (foodDir == "NE") && (curDir /= "S") && (checkDirection "N") = "N"
    | (foodDir == "NE") && (curDir /= "W") && (checkDirection "E") = "E"
    | (foodDir == "NE") && (curDir /= "N") && (checkDirection "S") = "S"
    | (foodDir == "NE") && (curDir /= "E") && (checkDirection "W") = "W"
    -- Food is South-East
    | (foodDir == "SE") && (curDir == "S") && (checkDirection "S") = "S"
    | (foodDir == "SE") && (curDir == "E") && (checkDirection "E") = "E"
    | (foodDir == "SE") && (curDir /= "N") && (checkDirection "S") = "S"
    | (foodDir == "SE") && (curDir /= "W") && (checkDirection "E") = "E"
    | (foodDir == "SE") && (curDir /= "S") && (checkDirection "N") = "N"
    | (foodDir == "SE") && (curDir /= "E") && (checkDirection "W") = "W"
    -- Food is South-West
    | (foodDir == "SW") && (curDir == "S") && (checkDirection "E") = "S"
    | (foodDir == "SW") && (curDir == "W") && (checkDirection "W") = "W"
    | (foodDir == "SW") && (curDir /= "N") && (checkDirection "S") = "S"
    | (foodDir == "SW") && (curDir /= "E") && (checkDirection "W") = "W"
    | (foodDir == "SW") && (curDir /= "S") && (checkDirection "N") = "N"
    | (foodDir == "SW") && (curDir /= "W") && (checkDirection "E") = "E"
    -- Food is North-West
    | (foodDir == "NW") && (curDir == "N") && (checkDirection "N") = "N"
    | (foodDir == "NW") && (curDir == "W") && (checkDirection "W") = "W"
    | (foodDir == "NW") && (curDir /= "S") && (checkDirection "N") = "N"
    | (foodDir == "NW") && (curDir /= "E") && (checkDirection "W") = "W"
    | (foodDir == "NW") && (curDir /= "N") && (checkDirection "S") = "S"
    | (foodDir == "NW") && (curDir /= "W") && (checkDirection "E") = "E"
    -- There is no valid direction
    | otherwise = "N/A"
    where
        curDir = (snakeDir state)
        curSnake = (snakeLoc state)
        checkDirection direction = not (snakeDeath (moveSnake direction curSnake))

-- Calculates the position of the food relative to the snake
greedyArrowDirection :: GameState -> [Char]
greedyArrowDirection state
    -- Case 1: Food is straight from snake
    | (xDiff == 0) && (yDiff > 0) = greedyStep "N" state
    | (xDiff == 0) && (yDiff < 0) = greedyStep "S" state
    | (xDiff > 0) && (yDiff == 0) = greedyStep "E" state
    | (xDiff < 0) && (yDiff == 0) = greedyStep "W" state
    -- Case 2: Food is diagonal from snake
    | (xDiff > 0) && (yDiff > 0) = greedyStep "NE" state
    | (xDiff < 0) && (yDiff > 0) = greedyStep "NW" state
    | (xDiff > 0) && (yDiff < 0) = greedyStep "SE" state
    | (xDiff < 0) && (yDiff < 0) = greedyStep "SW" state
    | otherwise = "N/A"
    where
        foodX = fst (foodLoc state)
        foodY = snd (foodLoc state)
        snakeHead = head (snakeLoc state)
        snakeX = fst snakeHead
        snakeY = snd snakeHead
        xDiff = foodX - snakeX
        yDiff = foodY - snakeY

-- Takes three directions and gives the direction that can be traveled the most
-- without dying. The inputs have descending priority
smartestDirection :: [Char] -> [Char] -> [Char] -> [(Int, Int)] -> [Char]
smartestDirection dir1 dir2 dir3 snake
    -- Prioritize curDir is possible
    | (validDst1 == 0) && (validDst2 == 0) && (validDst3 == 0) = "N/A"
    | (validDst1 >= validDst2) && (validDst1 >= validDst3) = dir1
    | (validDst2 >= validDst1) && (validDst2 >= validDst3) = dir2
    | (validDst3 >= validDst1) && (validDst3 >= validDst2) = dir3
    where
        validDst1 = snakePrediction dir1 snake
        validDst2 = snakePrediction dir2 snake
        validDst3 = snakePrediction dir3 snake

-- Given a direction and game state, calculates the smartest direction the snake can go
smartStep :: [Char] -> GameState -> [Char]
smartStep foodDir state
    -- If the food is only in one direction from the food (N, E, S, W):
    -- 1. If the food is only in one direction from the snake, we try to go in that direction if it is smart
    -- 2. If there isn't a direct path to food, try to go in the smartest valid direction
    -- Food is North
    | (foodDir == "N") && (curDir /= "S") && (smartCheck "N" foodDstY) = "N"
    | (foodDir == "N") && (curDir == "N") = smartestDirection "E" "W" "N" snake
    | (foodDir == "N") && (curDir == "E") = smartestDirection "N" "S" "E" snake
    | (foodDir == "N") && (curDir == "S") = smartestDirection "E" "W" "S" snake
    | (foodDir == "N") && (curDir == "W") = smartestDirection "N" "S" "W" snake
    -- Food is East
    | (foodDir == "E") && (curDir /= "W") && (smartCheck "E" foodDstX) = "E"
    | (foodDir == "E") && (curDir == "E") = smartestDirection "N" "S" "E" snake
    | (foodDir == "E") && (curDir == "S") = smartestDirection "E" "W" "S" snake
    | (foodDir == "E") && (curDir == "W") = smartestDirection "N" "S" "W" snake
    | (foodDir == "E") && (curDir == "N") = smartestDirection "E" "W" "N" snake
    -- Food is South
    | (foodDir == "S") && (curDir /= "N") && (smartCheck "S" foodDstY) = "S"
    | (foodDir == "S") && (curDir == "S") = smartestDirection "E" "W" "S" snake
    | (foodDir == "S") && (curDir == "W") = smartestDirection "S" "N" "W" snake
    | (foodDir == "S") && (curDir == "N") = smartestDirection "E" "W" "N" snake
    | (foodDir == "S") && (curDir == "E") = smartestDirection "S" "N" "E" snake
    -- Food is West
    | (foodDir == "W") && (curDir /= "E") && (smartCheck "W" foodDstX) = "W"
    | (foodDir == "W") && (curDir == "W") = smartestDirection "N" "S" "W" snake
    | (foodDir == "W") && (curDir == "N") = smartestDirection "W" "E" "N" snake
    | (foodDir == "W") && (curDir == "E") = smartestDirection "N" "S" "E" snake
    | (foodDir == "W") && (curDir == "S") = smartestDirection "W" "E" "S" snake
    -- No straight path
    -- If the food is in two direction from the food (NE, SE, SW, NW):
    -- 1. If the snake is already going in one of the directions of the food, attempt to stay on it
    -- 2. If the snake can move in one of the directions of the food, attempt to turn to it
    -- 3. If the snake can't go directly in one of the directions of the food, try smartest direction
    -- Food is North-East
    | (foodDir == "NE") && (curDir == "N") && (smartCheck "N" foodDstY) = "N"
    | (foodDir == "NE") && (curDir == "E") && (smartCheck "E" foodDstX) = "E"
    | (foodDir == "NE") && (curDir /= "S") && (smartCheck "N" foodDstY) = "N"
    | (foodDir == "NE") && (curDir /= "W") && (smartCheck "E" foodDstX) = "E"
    | (foodDir == "NE") && (curDir == "N") = smartestDirection "E" "W" "N" snake
    | (foodDir == "NE") && (curDir == "E") = smartestDirection "N" "S" "E" snake
    | (foodDir == "NE") && (curDir == "S") = smartestDirection "E" "W" "S" snake
    | (foodDir == "NE") && (curDir == "W") = smartestDirection "N" "S" "W" snake
    -- Food is South-East
    | (foodDir == "SE") && (curDir == "S") && (smartCheck "S" foodDstY) = "S"
    | (foodDir == "SE") && (curDir == "E") && (smartCheck "E" foodDstX) = "E"
    | (foodDir == "SE") && (curDir /= "N") && (smartCheck "S" foodDstY) = "S"
    | (foodDir == "SE") && (curDir /= "E") && (smartCheck "E" foodDstX) = "E"
    | (foodDir == "SE") && (curDir == "N") = smartestDirection "E" "W" "N" snake
    | (foodDir == "SE") && (curDir == "E") = smartestDirection "S" "N" "E" snake
    | (foodDir == "SE") && (curDir == "S") = smartestDirection "E" "W" "S" snake
    | (foodDir == "SE") && (curDir == "W") = smartestDirection "S" "N" "W" snake
    -- Food is South-West
    | (foodDir == "SW") && (curDir == "S") && (smartCheck "S" foodDstY) = "S"
    | (foodDir == "SW") && (curDir == "W") && (smartCheck "W" foodDstX) = "W"
    | (foodDir == "SW") && (curDir /= "N") && (smartCheck "S" foodDstY) = "S"
    | (foodDir == "SW") && (curDir /= "E") && (smartCheck "W" foodDstX) = "W"
    | (foodDir == "SW") && (curDir == "N") = smartestDirection "W" "E" "N" snake
    | (foodDir == "SW") && (curDir == "E") = smartestDirection "S" "N" "E" snake
    | (foodDir == "SW") && (curDir == "S") = smartestDirection "W" "E" "S" snake
    | (foodDir == "SW") && (curDir == "W") = smartestDirection "S" "N" "W" snake
    -- Food is North-West
    | (foodDir == "NW") && (curDir == "N") && (smartCheck "N" foodDstY) = "N"
    | (foodDir == "NW") && (curDir == "W") && (smartCheck "W" foodDstX) = "W"
    | (foodDir == "NW") && (curDir /= "S") && (smartCheck "N" foodDstY) = "N"
    | (foodDir == "NW") && (curDir /= "E") && (smartCheck "W" foodDstX) = "W"
    | (foodDir == "NW") && (curDir == "N") = smartestDirection "W" "E" "N" snake
    | (foodDir == "NW") && (curDir == "E") = smartestDirection "N" "S" "E" snake
    | (foodDir == "NW") && (curDir == "S") = smartestDirection "W" "E" "S" snake
    | (foodDir == "NW") && (curDir == "W") = smartestDirection "N" "S" "W" snake
    | otherwise = "N/A"
    where
        curDir = (snakeDir state)
        snake = snakeLoc state
        snakeHead = head (snakeLoc state)
        food = foodLoc state
        foodDstX = abs ((fst food) - (fst snakeHead))
        foodDstY = abs ((snd food) - (snd snakeHead))
        -- snakePrediction gives us the number of steps we can take in a particular direction before game over
        -- If the number of steps is greater than or equal to distance, that means we can reach goal without dying
        smartCheck direction distance
            | ((snakePrediction direction snake) >= distance) = True
            | otherwise = False

-- Calculates the position of the food relative to the snake
smartArrowDirection :: GameState -> [Char]
smartArrowDirection state
    -- Case 1: Food is straight from snake
    | (xDiff == 0) && (yDiff > 0) = smartStep "N" state
    | (xDiff == 0) && (yDiff < 0) = smartStep "S" state
    | (xDiff > 0) && (yDiff == 0) = smartStep "E" state
    | (xDiff < 0) && (yDiff == 0) = smartStep "W" state
    -- Case 2: Food is diagonal from snake
    | (xDiff > 0) && (yDiff > 0) = smartStep "NE" state
    | (xDiff < 0) && (yDiff > 0) = smartStep "NW" state
    | (xDiff > 0) && (yDiff < 0) = smartStep "SE" state
    | (xDiff < 0) && (yDiff < 0) = smartStep "SW" state
    | otherwise = "N/A"
    where
        foodX = fst (foodLoc state)
        foodY = snd (foodLoc state)
        snakeHead = head (snakeLoc state)
        snakeX = fst snakeHead
        snakeY = snd snakeHead
        xDiff = foodX - snakeX
        yDiff = foodY - snakeY

-- Check and update the game status
updateGame :: Float -> GameState -> GameState
updateGame second game
  |(menu game) == 0 || (menu game) == 1 = game
  -- check if snake dead
  | snakeDeath (snakeLoc game) = game {menu = 0}
  -- check if snake hits obstacles
  | head(snakeLoc game) == (poisonLoc1 game) = game { menu = 0 }
  | head(snakeLoc game) == (poisonLoc2 game) = game { menu = 0 }
  | head(snakeLoc game) == (poisonLoc3 game) = game { menu = 0 }
  --snake eats food
  | head(snakeLoc game) == (foodLoc game) = game
             { foodLoc = genFood,
               score = addFunc,
               snakeLoc = (head nextLoc):(snakeLoc game),
               greedyArrow = greedyArrowDirection game,
               smartArrow = smartArrowDirection game,
               multiplier = (multiplier game) + 1
             }
    --nothing happens, update arrow and snake color
  | (snakeColor game) == green = game {snakeColor = green}
  | otherwise = game { snakeColor = green,
                       greedyArrow = greedyArrowDirection game,
                       smartArrow = smartArrowDirection game
                     }
   where
   nextLoc = temp (snakeDir game) 2 (snakeLoc game)
   addFunc = (score game) + (div (multiplier game) 5) * 100
   -- if food location is the same as poison food, then regenerate the food
   genFood
        | food == (160, 160) = unsafePerformIO makeFood
        | food == (-160, 160) = unsafePerformIO makeFood
        | food == (160, -160) = unsafePerformIO makeFood
        | otherwise = food
            where
                food = unsafePerformIO makeFood
   -- generate random food location
   makeFood = 
       do
        x <- randomRIO ((-(div width 2)) + 2, (div width 2) - 2)
        y <- randomRIO ((-(div width 2)) + 2, (div height 2) - 2)
        return ((x * grid), (y * grid))

main :: IO ()
main = play gameUI background speed initialState render handleKeys updateGame