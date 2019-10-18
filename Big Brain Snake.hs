module Main(main, GameState, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import System.IO.Unsafe
import Debug.Trace

grid, width, height, speed :: Int
grid = 20
width = 50
height = 50
offset = 100
speed = 5

maxX, minX, maxY, minY :: Int
maxY = grid * (div height 2)
minY = grid * (-(div height 2))
maxX = grid * (div width 2)
minX = grid * (-(div width 2))

gameUI :: Display
gameUI = InWindow "312" (width * grid, height * grid) (offset, offset)

background, wallColor :: Color
background = blue
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
   } deriving Show


initialState :: GameState
initialState = Game
  { foodLoc = unsafePerformIO makeFood
  , snakeLoc = [(0, 0)]
  , snakeDir = "n"
  , greedyArrow = "N/A"
  , smartArrow = "N/A"
  , menu = 1
  , score = 0
  , snakeColor = green
  }
   where
     makeFood = 
       do 
          x <- randomRIO ((-(div width 2)) + 1, (div width 2) - 1)
          y <- randomRIO ((-(div width 2)) + 1, (div height 2) - 1)
          return (x * grid, y * grid)


render :: GameState -> Picture
render game 
 | (menu game) == 1 = pictures [startScreen]
 | (menu game) == 0 = pictures [endScreen]
 | otherwise = pictures [food, snake, gArrow, sArrow, wallW, wallN, wallE, wallS, scoreBoard]
 where
-- I don’t know the exact size. After we run it, we can adjust the 30.
      startScreen = Scale 5 5 (Text "Press 1 to start the game or Q anytime to quit!")
      endScreen = Scale 5 5 (Text "GameOver! Press Q to quit or 1 to play again!")
--food
      food = uncurry translate loc $ color foodColor $ rectangleSolid 10 10
      loc = (fromIntegral (fst (foodLoc game)), fromIntegral (snd (foodLoc game)))
--snake
      snake = drawHelper ([ (fromIntegral(x), fromIntegral(y)) | (x, y) <- snakeLoc game])
      drawHelper lst = pictures (drawPart lst (snakeColor game))
              where
                    drawPart [ ] _ = [ ]
                    drawPart (h:t) c = (renderPart h c):(drawPart t c)
                    renderPart (x, y) c = color c $ translate x y (circleSolid 
                                          (fromIntegral grid))
--wall
      wallW = color wallColor $ translate (fromIntegral maxX) 0 (rectangleSolid (fromIntegral grid) wallHeight)
      wallE = color wallColor $ translate (fromIntegral minX) 0 (rectangleSolid (fromIntegral grid) wallHeight)
      wallN = color wallColor $ translate 0 (fromIntegral maxY) (rectangleSolid wallWidth (fromIntegral grid))
      wallS = color wallColor $ translate 0 (fromIntegral minY) (rectangleSolid wallWidth (fromIntegral grid))
      wallWidth = fromIntegral (width * grid)
      wallHeight = fromIntegral (height * grid)

--Score
      scoreBoard = uncurry translate scoreLoc $ color white $ Scale 2 2 (Text (show (score game)))
      scoreLoc = (fromIntegral (div (width * grid) 2), fromIntegral (div (height*grid - maxY)  2))

--Arrow
      gArrow
          | (greedyArrow game) == (smartArrow game) = arrowFunction violet (snakeDir game)
          | otherwise = arrowFunction gArrowColor (snakeDir game)
            where
              arrowFunction c dir
                | dir == "n" || dir == "s" = uncurry translate (x + z, y) $ color c $ 
                   Text (greedyArrow game)
                | dir == "e" || dir == "w" = uncurry translate (x, y + z) $ color c $ 
                   Text (greedyArrow game)                   
                    where
                      x = fromIntegral (fst (head (snakeLoc game)))
                      y = fromIntegral (snd (head (snakeLoc game)))
                      z = fromIntegral grid
      sArrow
          | (greedyArrow game) == (smartArrow game) = gArrow
          | otherwise = arrowFunction2 sArrowColor (snakeDir game)
            where
              arrowFunction2 c dir
                | dir == "n" || dir == "s" = uncurry translate (x - z, y) $ color c $ 
                   Text (smartArrow game)
                | dir == "e" || dir == "w" = uncurry translate (x, y - z) $ color c $ 
                   Text (smartArrow game)                   
                    where
                      x = fromIntegral (fst (head (snakeLoc game)))
                      y = fromIntegral (snd (head (snakeLoc game)))
                      z = fromIntegral grid


update :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
update _ _ [ ] = [ ]
update x y lst = (x, y):(init lst)

update2 :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
update2 _ _ [ ] = [ ]
update2 x y (h:t) = (x, y):(update2 (fst h) (snd h) t)

temp :: String -> Int -> [(Int, Int)] -> [(Int, Int)]
temp z d loc
 | z == "n" && d == 1 = update x (y + grid) loc
 | z == "s" && d == 1 = update x (y - grid) loc
 | z == "e" && d == 1 = update (x + grid) y loc
 | z == "w" && d == 1 = update (x - grid) y loc
 | z == "n" && d == 2 = update2 x (y + grid) loc
 | z == "s" && d == 2 = update2 x (y - grid) loc
 | z == "e" && d == 2 = update2 (x + grid) y loc
 | z == "w" && d == 2 = update2 (x - grid) y loc
   where 
     x = fromIntegral (fst (head loc))
     y = fromIntegral (snd  (head loc))

handleKeys (EventKey (Char key) _ _ _) game 
 | key == 'w' && (snakeDir game) /= "s"  = game {snakeLoc = temp "n" 1 (snakeLoc game), snakeDir = "n"}
 | key == 's' && (snakeDir game) /= "n"  = game {snakeLoc = temp "s" 1 (snakeLoc game), snakeDir = "s"}
 | key == 'a' && (snakeDir game) /= "w"  = game {snakeLoc = temp "e" 1 (snakeLoc game), snakeDir = "e"}
 | key == 'd' && (snakeDir game) /= "e"  = game {snakeLoc = temp "w" 1 (snakeLoc game), snakeDir = "w"}
 | (menu game) == 1 && key == '1' = initialState { menu = 2 }
 | (menu game) == 0 && key == '1' = game { menu = 1 }
 | key == 'q' =  game { snakeLoc = [ ] }
 | otherwise  = game
     

handleKeys (EventKey (SpecialKey key) _ _ _) game
 | key == KeyUp && (snakeDir game) /= "s" = game {snakeLoc = temp "n" 1 (snakeLoc game), snakeDir = "n"}
 | key == KeyDown && (snakeDir game) /= "n" = game {snakeLoc = temp "s" 1 (snakeLoc game), snakeDir = "s"}
 | key == KeyRight && (snakeDir game) /= "w" = game {snakeLoc = temp "e" 1 (snakeLoc game), snakeDir = "e"}
 | key == KeyLeft && (snakeDir game) /= "e" = game {snakeLoc = temp "w" 1 (snakeLoc game), snakeDir = "w"}
 | key == KeyEsc = game { menu = 1 }
 | otherwise = game

handleKeys _ game = game

checkIntersect :: (Int, Int) -> [(Int, Int)] -> Bool
checkIntersect snakeHead [] = False
checkIntersect snakeHead (h:t)
   | (snakeHead == h) = True
   | otherwise = checkIntersect snakeHead t

checkBoundary :: (Int, Int) -> Bool
checkBoundary (x, y)
   | (x < minX) || (x > maxX) || (y < minY) || (y > maxY) = True
   | otherwise = False

-- Given snakeLoc, checks if the snake touches itself
snakeDeath :: [(Int, Int)] -> Bool
snakeDeath (h:t) = (checkIntersect h t) && (checkBoundary h)

updateGame :: Float -> GameState -> GameState
updateGame second game
  |(menu game) == 0 || (menu game) == 1 = game
  -- check if snake dead
  | checkBoundary (head (snakeLoc game)) =  game { menu = 0 }
  --snake eats food
  | checkIntersect (head (snakeLoc game)) (tail (snakeLoc game)) = game { menu = 0 }
  | head nextLoc == foodLoc game = game
             { foodLoc = unsafePerformIO makeFood,
               score = addFunc,
               snakeLoc = (head nextLoc):(snakeLoc game)
             }
    --nothing happens
  | (snakeColor game) == green = game { snakeColor = red }
  | otherwise = game { snakeColor = green }
   where
   nextLoc = temp (snakeDir game) 2 (snakeLoc game)
   addFunc = (score game) + 100
   makeFood = 
       do
        x <- randomRIO ((-(div width 2)) + 1, (div width 2) - 1)
        y <- randomRIO ((-(div width 2)) + 1, (div height 2) - 1)
        return (x * grid, y * grid)

main :: IO ()
main = play gameUI background speed initialState render handleKeys updateGame