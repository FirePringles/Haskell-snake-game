module Main(main) where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

import System.Random

-- The size of the grid
-- INVARIANT: Only positive integers
width, height, squareSide :: Float
width      = 25
height     = 15
squareSide = 40

-- The actual size of the window, and translation constants x0, y0
pixelWidth = squareSide * width
x0 = (-pixelWidth/2)
pixelHeight = squareSide * height
y0 = (-pixelHeight/2)


window :: Display
window = InWindow "Snake" ((floor pixelWidth), (floor pixelHeight)) (0,0)


foodIndex = 10



------------------------------------------------------------------------------
--DATA TYPES
------------------------------------------------------------------------------
{- SnakeGame 
   Represents a game state (Game Board positions foodList) where
   Board contains all the spaces of the board,
   positions represents all snakes in play and
   foodList the location of the food.
-}
data SnakeGame = Game Board [[Pos]] [Float] deriving (Show)


{- Pos
   Represents a piece of a snake (c, dir, pos), where
   c gives the color of the snake,
   dir the direction the piece is headed and
   pos its position on the board.
-}
type Pos = (Color, Direction ,Float)     


{- Board
   Describes the board of the current game state, with a certaian number
   of spaces, each containing a number of objects.
   INVARIANT: The Snake objects must represent a valid snake, where all the
              segments form a line, and the direction of every segment
              in the tail points to the next segment towards the head
-}
type Board = [Space]


{- Space
   Describes a space on the board, which holds a number of objects
-}
type Space = [Object]


{- Object
   A data type for describing the objects of a space on the board.
   An Object can be a Snake, a piece of Food or Void.
   The Snake object has a direction and a color.
-}
data Object = Snake Direction Color | Food | Void deriving (Eq, Show)




{- Direction
   Describes the direction of a Snake Object,
   where N means it is headed North, S south, W west, E east.
-}
data Direction = N | S | W | E deriving (Show, Eq)


{- Change
   Represents the instructions for changing a space on the board, either
   Writing a snake with a direction and a color to a certain spot on the board,
   Erasing a snake with a direction and a color from a spot on the board,
   Writing a piece of food to a spot on the board or
   Erasing a piece of food from a spot on the bord
-}
data Change = Write Direction Color Float | Erase Direction Color Float | WriteFood Float | EraseFood Float  deriving (Eq)
------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- GRID
-------------------------------------------------------------------------------
{- vertiGrid i acc
   Creates the vertical lines of a grid with (width) number of squares along the x-axis
   RETURNS:(width-1) vertical lines, from top to bottom, spaced out evenly in the window
   EXAMPLES: vertiGrid 1 []
-}
vertiGrid :: Float -> [Picture] -> [Picture]
vertiGrid i acc
 | i >= width = acc
 | otherwise =  vertiGrid (i+1) ((Line [(x, y0),(x, (-y0))]) : acc)
     where x = x0 + i*squareSide


{- horiGrid i acc
   Creates the horisontal lines of a grid with (height) number of squares along the y-axis
   RETURNS:(height-1) horisontal lines, from far left to far right, spaced out evenly in the window
   EXAMPLES: horiGrid 1 []
-}
horiGrid :: Float -> [Picture] -> [Picture]
horiGrid i acc
  | i >= height = acc
  | otherwise = horiGrid (i+1) ((Line [(x0, y),((-x0), y)]) : acc)
      where y = y0 + i*squareSide


{- grid
   Creates a grid of size (width x height), which fully covers the window
   RETURNS: The lines of a grid of size (width x height), where each square is of size (squareSide x squareSide)
-}
grid :: [Picture]
grid = vertiGrid 1 (horiGrid 1 [])
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- RENDERING
------------------------------------------------------------------------------
{- render gs
   Creates a picture of the game state gs
   RETURNS: The rendering of all objects in gs
-}
render :: SnakeGame -> Picture
render (Game board _ _) = pictures (renderBoard board (1,1) (grid))

{- renderBoard board (x,y) acc
   Creates pictures of all objects in board
   RETURNS: The rendering of all objects in board
   EXAMPLES: renderBoard board (1,1) []
-}
renderBoard :: Board -> (Float, Float) -> [Picture] -> [Picture]
renderBoard [] _ acc = acc
renderBoard (s:spaces) (x,y) acc
  | x >= width = renderBoard spaces (1, (y+1)) (renderSpace s (x,y) acc)
  | otherwise = renderBoard spaces ((x+1), y) (renderSpace s (x,y) acc)

{- renderSpace s (x,y) acc
   Creates a picture of the first object in s, translates it into place and adds it to acc
   RETURNS: A rendering of s, followed by acc
-}
renderSpace ::  Space -> (Float, Float) -> [Picture] -> [Picture]
renderSpace ((Snake d c):objects) (x,y) acc = (color c (translate (x0 + (x-0.5)*squareSide) (y0 + (y-0.5)*squareSide) snakeDrawing)) : acc
renderSpace (Void:objects) (x,y) acc = acc
renderSpace (Food:objects) (x,y) acc = (translate (x0 + (x-0.5)*squareSide) (y0 + (y-0.5)*squareSide) foodDrawing) : acc


snakeDrawing = rectangleSolid squareSide squareSide
foodDrawing = rectangleSolid (squareSide*0.5) (squareSide*0.5)
------------------------------------------------------------------------------




------------------------------------------------------------------------------
-- UPDATE BOARD
------------------------------------------------------------------------------
{- moveSnakes seconds gs
   Updates gs by moving all snakes according to their directions and checking if they collide with food
   RETURNS: an updated game state
   EXAMPLES: ...
-}
moveSnakes :: Float -> SnakeGame -> SnakeGame
moveSnakes seconds (Game board (p1:positions) foods) = Game (updateBoard updatedBoard 1 (changeSort changes)) newPositions newFoods
  where (updatedAiPos, updatedBoard) = updateComputerPos board positions foods [] 
        (changes, newPositions, newFoods) = changeList (p1:(updatedAiPos)) [] [] foods

updateComputerPos :: Board -> [[Pos]] -> [Float] -> [[Pos]] -> ([[Pos]], Board)
updateComputerPos board [] foods posAcc = (posAcc, board)
updateComputerPos board (((c, dir, pos):p):positions) foods posAcc = updateComputerPos (changeDirSpace (c, newDir, pos) board 1) positions foods (((c, newDir, pos):p):posAcc)
  where newDir = computeAiMove board ((c, dir, pos):p) foods





{-
changeDirSpace pos board i
computeAiMove :: Board -> [Pos] -> [Float] -> Direction
-}

{- updateBoard board pos changeList
   Updates the board according to the changes in changeList
   PRE: changeList is sorted so all changes have increasing positions
        no changes may result in a snake moving of the board
   RETURNS: board changed according to changeList
   EXAMPLES: updateBoard [[Void],[Void],[Snake S red],[Snake W red]] 1 [(Write S red 1), (Erase W red 4)] = [[Snake S Red, Void], [Void], [Snake S Red], [Void]]
             (on a 2x2 board)
-}
updateBoard :: Board -> Float -> [Change] -> Board
updateBoard board i [] = board

updateBoard (s:spaces) i change@((Write dir c pos):changes)
  | i /= pos = s: (updateBoard spaces (i+1)change)
updateBoard (s:spaces) i change@((Erase dir c pos):changes)
  | i /= pos = s: (updateBoard spaces (i+1)change)
updateBoard (s:spaces) i change@((WriteFood pos):changes)
  | i /= pos = s: (updateBoard spaces (i+1)change)
updateBoard (s:spaces) i change@((EraseFood pos):changes)
  | i /= pos = s: (updateBoard spaces (i+1)change)
  
updateBoard (s:spaces) i ((Write dir c pos):changes) = updateBoard (((Snake dir c):s):spaces) (i) changes
updateBoard (s:spaces) i ((WriteFood pos):changes) = updateBoard (((Food):s):spaces) (i) changes
updateBoard (s:spaces) i ((Erase dir c pos):changes) = updateBoard ((eraseAux s (Erase dir c pos)):spaces) (i) changes
updateBoard (s:spaces) i ((EraseFood pos):changes) = updateBoard ((eraseAux s (EraseFood pos)):spaces) (i) changes



{-eraseAux space eraseChange
  Erases the object in space specified by eraseChange
  PRE: The object specified by eraseChange is in space
  RETURNS: space without the object in eraseChange
  EXAMPLES: eraseAux [(Snake N blue),(Snake W red)] (Erase W red 12) = [Snake N blue, Void]
            eraseAux [(Snake, N, blue), Void] (Erase N Blue 12) = [Void]
-}
eraseAux :: Space -> Change -> Space
eraseAux ((Snake dir1 c1):objects) (Erase dir2 c2 pos)
  | (dir1 == dir2) && (c1 == c2) && (objects == []) = [Void]
  | (dir1 == dir2) && (c1 == c2) = objects
eraseAux (Food:objects) (EraseFood pos)
  | objects == [] = [Void]
  | otherwise = objects
eraseAux (o:objects) erase = o:(eraseAux objects erase)
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- CHANGE LIST
--------------------------------------------------------------------------------
{- changeList positions cAcc pAcc foodList
   Calculates all the changes to the board given by positions and foodList, and also gives a new list of positions and a new foodList
   RETURNS: The changes given by positions and foodList, the new positions, and the new foodList
   EXAMPLES: changeList [[(red, W, 13.0), (red, W, 14.0)]] [] [] [10,11] =
               = ([Write W red 12.0, Erase W red 14.0],[[(red, W, 12.0),(red, W, 13.0)]] [11.0, 10.0])
             changeList [[(red, W, 13.0), (red, W, 14.0)]] [] [] [10,13] =
	       = ([Write W red 12.0, EraseFood 13.0],[[(red, W, 12.0),(red, W, 13.0), (Red, W, 14.0)]] [10.0])   (on a 5x5 board)
-}
changeList :: [[Pos]] -> [Change] -> [[Pos]] -> [Float] -> ([Change], [[Pos]], [Float])
changeList [] changeAcc posAcc foods = (changeAcc, posAcc, foods)
changeList (p:pos) changeAcc posAcc foods = changeList pos (changeHead : changeTail : changeAcc) (posAcc ++ [newPos]) newFoods
  where (changeHead, changeTail, newPos, newFoods) = translatePos p foods



{- translatePos positions foodList
   Calculates what two changes of the board need to be performed considering positions and foodList, and also gives a new list of positions and foodList.
   RETURNS: The two changes of the board positions and foodList results in, the new list of positions and the new foodList
   EXAMPLES: translatePos [(red, W, 12), (red, W, 13)] [10, 5] = (Write W red 11.0 , Erase W red 13.0, [(red, W, 11.0), (red, W, 12.0)], [5.0, 10.0])
             translatePos [(red, W, 12), (red, W, 13)] [10, 12] = (Write W red 11.0 , EraseFood 12.0, [(red, W, 11.0), (red, W, 12.0), (red, W, 13.0)], [10.0])
             (on a 5x5 board)
-}
translatePos :: [Pos] -> [Float] -> (Change, Change, [Pos], [Float])
translatePos (p:positions) foods
  | foodCheck /= 0 = ((Write newHeadDir c newHeadPos), EraseFood oldTailPos, ((c, newHeadDir, newHeadPos):newTail), newFoods)
  | otherwise = ((Write newHeadDir c newHeadPos), (Erase oldTailDir c oldTailPos), ((c, newHeadDir, newHeadPos):newTail), newFoods)
  where ((c, newHeadDir, newHeadPos), newFoods, foodCheck) = translatePosHead p foods
        (newTail, (oldTailDir, oldTailPos)) = translatePosTail [] (p:positions) foodCheck



{- translatePosTail acc positions foodCheck
   Computes a new tail from positions, and also gives what should be erased, the last piece of the tail if foodCheck == 0, or the food in position foodCheck otherwise
   RETURNS: A new tail, and the coordinates of a tail to be erased, or a food coordinate.
   EXAMPLES: translatePosTail [] [(red, W, 12), (red, W, 13)] 0 = ([(red, W, 12.0)], (W, 13.0))
             translatePosTail [] [(red, W, 12), (red, W, 13)] 5 = ([(red, W, 12.0), (red, W, 13.0)], (N, 5.0))     (on a 5x5 board)
-}
translatePosTail :: [Pos] -> [Pos] -> Float -> ([Pos], (Direction, Float))
translatePosTail acc [(c1, dir1, pos1), (c2, dir2, pos2)] foodCheck
  | foodCheck == 0 = ((reverse ((c1, dir1, pos1):acc)), (dir2, pos2))
  | otherwise = ((reverse ((c2, dir2, pos2):(c1, dir1, pos1):acc)), (N, foodCheck))
translatePosTail acc (p:positions) foodCheck = translatePosTail (p:acc) positions foodCheck



{- translatePosHead head foodList
   Creates a new position for head based on its direction and checks if the head is in contact with food.
   RETURNS: A new position for head, a foodList where food in the same position as the head is removed, and the position of eventual removed food.
   EXAMPLES: translatePosHead (red, W, 14) [10,11] = ((red, W, 13.0), [11.0,10.0], 0.0)     
             translatePosHead (red, W, 14) [10, 14] = ((red, W, 13.0), [10.0], 14.0)       (on a 5x5 board)
-}
translatePosHead :: Pos -> [Float] -> (Pos, [Float], Float)
translatePosHead (c, dir, pos) foods
  | dir == N = ((c, dir, pos+width), newFoods, tailUpdate)
  | dir == S = ((c, dir, pos-width), newFoods, tailUpdate)
  | dir == W = ((c, dir, pos-1), newFoods, tailUpdate)
  | dir == E = ((c, dir, pos+1), newFoods, tailUpdate)
    where (newFoods, tailUpdate) = checkForFood pos foods []



{- checkForFood pos foodList acc
   Checks if pos is in foodList, and if so returns foodList without pos, paired with pos, otherwise the same foodList paired with 0 is returned.
   RETURNS: (foodList without pos, pos) if pos in foodList, otherwise (foodList, 0). The order of foodList is not preserved.
   EXAMPLES: checkForFood 4 [1,2,3] [] = ([3.0, 2.0, 1.0], 0.0)
             checkForFood 4 [3,4,5] [] = ([3.0, 5.0], 4.0)
-}
checkForFood :: Float -> [Float] -> [Float] -> ([Float], Float)
checkForFood _ [] acc = (acc, 0)
checkForFood headPos (f:foods) acc
  | headPos == f = (spawnNew (acc ++ foods),f)
  | otherwise = checkForFood headPos foods (f:acc)

spawnNew :: [Float] -> [Float]
spawnNew ys 
  | length ys < foodIndex = [5] ++ ys
  | otherwise = ys

emptySpaces :: Board -> ([Float], Float)
emptySpaces [] = ([], 0)
emptySpaces (x:xs) = (z, w) where
  z = emptySpacesAux (x:xs) 1.0
  w = fromIntegral (length z)
  
emptySpacesAux :: Board -> Float -> [Float]
emptySpacesAux [] _ = []
emptySpacesAux (x:xs) y
  | x == [Void] = y:emptySpacesAux xs (y+1.0)
  | otherwise = emptySpacesAux xs (y+1.0)

randomNumber :: Float -> [[Pos]] -> Float
randomNumber seconds pos = fromIntegral (round (seconds)*(length(pos !! 0)))

index :: Float
index = fromIntegral (mod (round (randomNumber 10.0 [[(red, N, 2),(red, N, 3),(red ,W, 4)],[]])) (round (snd (emptySpaces [[Void],[Food],[Void],[Food],[Void],[Void]]))))
 
newFood = (fst (emptySpaces [[Void],[Food],[Void],[Food],[Void],[Void]])) !! (round index)
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- AI
-------------------------------------------------------------------------------

computeAiMove :: Board -> [Pos] -> [Float] -> Direction
computeAiMove board positions@((c1, dir1, pos1):(c2, dir2, pos2):pos) foods = finalDir
  where (dirPriority, headx, heady) = directionPriority (closestFood (convertToCoord pos1) 999 foods) pos1
        borderx = illegalx headx
        bordery = illegaly heady
        backwards = illegalBackward positions
        preBoardCheck = removeIllegalDir bordery (removeIllegalDir borderx (removeIllegalDir backwards dirPriority))
        finalDir = finalRemoval preBoardCheck (checkBoard board (removeOutOfBounds [(pos1-width, S), (pos1-1, W), (pos1+1, E), (pos1+width, N)]) 1)


removeOutOfBounds :: [(Float, Direction)] -> [(Float, Direction)]
removeOutOfBounds [] = []
removeOutOfBounds ((pos, dir):dirs)
  | (pos < 1) || (pos > (width*height)) = removeOutOfBounds dirs
  | otherwise = (pos, dir) : removeOutOfBounds dirs


closestFood :: (Float, Float) -> Float -> [Float] -> Float
closestFood _ closest []  = closest
closestFood (headx, heady) closest (f:foods)
  | (sqrt((closestx - headx)^2 + (closesty - heady)^2)) <= (sqrt((foodx - headx)^2 + (foody - heady)^2)) = closestFood (headx, heady) closest foods
  | otherwise = closestFood (headx, heady) f foods
  where (closestx, closesty) = convertToCoord closest
        (foodx, foody) = convertToCoord f

finalRemoval :: [Direction] -> [Direction] -> Direction
finalRemoval (d:directions) [] = d
finalRemoval [d] _ = d
finalRemoval directions (i:illegals) = finalRemoval (removeIllegalDir [i] directions) illegals

-- pre on the board
checkBoard :: Board -> [(Float, Direction)] -> Float -> [Direction]
checkBoard [] _ _ = []
checkBoard _ [] _ = []
checkBoard (s:spaces) coordinates@((c, dir):coord) i
  | (i == c) && (checkForSnakes s) = dir : (checkBoard spaces coord (i+1))
  | (i == c) = checkBoard spaces coord (i+1)
  | otherwise = checkBoard spaces coordinates (i+1)

checkForSnakes :: Space -> Bool
checkForSnakes [] = False
checkForSnakes ((Snake _ _):spaces) = True
checkForSnakes (s:spaces) = checkForSnakes spaces

-- pre: unique dirs in directions
removeIllegalDir :: [Direction] -> [Direction] -> [Direction]
removeIllegalDir [] directions = directions
removeIllegalDir _ [] = []
removeIllegalDir (i:illegal) (d:directions)
  | i == d = directions
  | otherwise = d: (removeIllegalDir (i:illegal) directions)


illegalBackward :: [Pos] -> [Direction]
illegalBackward (p1:(c, dir, pos):positions)
  | dir == N = [S]
  | dir == S = [N]
  | dir == E = [W]
  | dir == W = [E]
  

illegaly :: Float -> [Direction]
illegaly heady
  | heady >= height = [N]
  | heady <= 1 = [S]
  | otherwise = []

illegalx :: Float -> [Direction]
illegalx headx
  | modWidth == 1 = [W]
  | modWidth == 0 = [E]
  | otherwise = []
  where modWidth =  mod (floor headx) (floor width)

directionPriority :: Float -> Float -> ([Direction], Float, Float)
directionPriority food head
  | (abs xDistance) >= (abs yDistance) = ((priorityList "x" xDistance yDistance), headx, heady)
  | otherwise = ((priorityList "y" xDistance yDistance), headx, heady)
  where (foodx, foody) = convertToCoord food
        (headx, heady) = convertToCoord head
        (xDistance, yDistance) = ((foodx - headx), (foody - heady))

priorityList d x y
  | (d == "x") && (x <= 0) && (y <= 0) = [W, S, N, E]
  | (d == "x") && (x <= 0) && (y > 0) = [W, N, S, E]
  | (d == "x") && (x > 0) && (y <= 0) = [E, S, N, W]
  | (d == "x") && (x > 0) && (y > 0) = [E, N, S, W]
  | (d == "y") && (x <= 0) && (y <= 0) = [S, W, E, N]
  | (d == "y") && (x <= 0) && (y > 0) = [N, W, E, S]
  | (d == "y") && (x > 0) && (y <= 0) = [S, E, W, N]
  | (d == "y") && (x > 0) && (y > 0) = [N, E, W, S]

convertToCoord :: Float -> (Float, Float)
convertToCoord i
  | modWidth == 0 = (width, fromIntegral (div int intWidth))
  | otherwise = ((fromIntegral modWidth), (fromIntegral (div int intWidth)+1))
  where int = floor i
        intWidth = floor width
        modWidth = mod int intWidth

-------------------------------------------------------------------------------




-------------------------------------------------------------------------------
-- HANDLING INPUT
-------------------------------------------------------------------------------
{- handleKeys event gs
   Checks for keyboard input and changes the direction of the first snakes head accordingly
   RETURNS: the same game state, except the direction of the first snakes head has been changed, if the input was valid. Otherwise the same gs
   EXAMPLES: ...
-}
handleKeys :: Event -> SnakeGame -> SnakeGame
handleKeys (EventKey (Char 'w') _ _ _) (Game board (((c, dir, square):pos):positions) foods) = Game (changeDirSpace (c, N, square) board 1) (((c, N, square):pos):positions) foods
handleKeys (EventKey (Char 'a') _ _ _) (Game board (((c, dir, square):pos):positions) foods) = Game (changeDirSpace (c, W, square) board 1) (((c, W, square):pos):positions) foods
handleKeys (EventKey (Char 's') _ _ _) (Game board (((c, dir, square):pos):positions) foods) = Game (changeDirSpace (c, S, square) board 1) (((c, S, square):pos):positions) foods
handleKeys (EventKey (Char 'd') _ _ _) (Game board (((c, dir, square):pos):positions) foods) = Game (changeDirSpace (c, E, square) board 1) (((c, E, square):pos):positions) foods
handleKeys _ game = game



{- changeDirSpace pos board i
   Changes the direction of snake head pos on board
   RETURNS: board, but the snake head given by pos has a direction given by pos
   EXAMPLES: changeDirSpace (red, E, 1) [[Snake S red],[Void],[Snake S red],[Void]] 1 = [[Snake E red],[Void],[Snake S red],[Void]]     (on a 2x2 board)
-}
changeDirSpace :: Pos -> Board -> Float -> Board
changeDirSpace (c, dir, square) (s:spaces) i
  | i == square = (changeDirSpaceAux (c, dir, square) s):spaces
  | otherwise = s:(changeDirSpace (c, dir, square) spaces (i+1))



{- changeDirSpaceAux pos objects
   Steps through objects and changes the snake with the same color as pos to pos
   REURNS: objects, where the snake with the same color as pos is changed to pos
   EXAMPLES: changeDirSpaceAux (red, N, 13) [(Snake S blue), (Snake W red)] = [Snake S blue, Snake N red]
-}
changeDirSpaceAux (c1, dir1, square) ((Snake dir2 c2):objects)
  | c1 == c2 = (Snake dir1 c1):objects
changeDirSpaceAux pos (o:objects) = o:(changeDirSpaceAux pos objects)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- INITIAL GAME STATE
------------------------------------------------------------------------------
initialGamestate = Game board pos [] 
  where
    (board, pos) = initialAux (genFromSize width height) [] 4 0

-- pre <= 4 snakes
initialAux :: Board -> [[Pos]] -> Int -> Int -> (Board, [[Pos]])
initialAux b@(s:spaces) pos snakes actualsnakes
  | snakes == actualsnakes = (b,pos)
  | otherwise = initialAux (oldspaces ++ newboard) newpos snakes (actualsnakes+1) 
    where    
      (newspaces,oldspaces) = goTo b (actualsnakes+1) 1 []
      (newboard, newpos) = insertSnake newspaces pos (actualsnakes+1)  
--genFromSize :: Float -> Float ->Board -> Board
genFromSize wid hig = replicate ((floor wid) * (floor hig)) [Void]

--insertSnake :: Board -> [[Pos]] -> Int -> (Board, [[Pos]])
insertSnake (s:s1:spaces) pos corner
  | s == [Void] && corner == 1 = ([(Snake E red)]:[(Snake E red)]:spaces, pos ++ [[(red, E, 2), (red,E,1)]])
  | s == [Void] && corner == 2 = ([(Snake E blue)]:[(Snake N blue)]:spaces, pos ++ [[(blue, N, width), (blue, E, (width - 1))]])
  | s == [Void] && corner == 3 = ([(Snake S green)]:[(Snake W green)]:spaces, pos ++ [[(green, S, (((height-1)* width)+1)), (green, W, ((height-1) * width)+2)]])
  | s == [Void] && corner == 4 = ([(Snake W orange)]:[(Snake W orange)]:spaces, pos ++ [[(orange, W, ((width*height)-1)), (orange, W, (width*height))]])


goTo :: Board -> Int -> Float -> Board -> (Board, Board)
goTo board@(s:spaces) corner index boardacc
  | corner == 1 && index == 1 = (board, (reverse boardacc))
  | corner == 2 && index == (width-1) = (board, (reverse boardacc))
  | corner == 2 && index /= (width-1) = (goTo spaces corner (index+1) (s:boardacc))
  | corner == 3 && index == (((height-1) * width) +1) = (board, (reverse boardacc))
  | corner == 3 && index /= (((height-1) * width) +1) = (goTo spaces corner (index+1) (s:boardacc))
  | corner == 4 && index == ((width*height)-1) = (board, (reverse boardacc))
  | corner == 4 && index /= ((width*height)-1) = (goTo spaces corner (index+1) (s:boardacc))
---------------------------------------------------------------------------




update = moveSnakes

main = play window white 1 (Game
 [[Snake E blue],[Snake E blue],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],
 [Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],
 [Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],
 [Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Snake W green],[Snake W green],
 [Void],[Void],[Void],[Void],[Void],[Void],[Snake S orange],[Snake W orange],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],
 [Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],
 [Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],
 [Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],
 [Snake E red],[Snake E red],[Snake E red],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void]]
 [[(red, E, 131), (red, E, 130), (red, E, 129)], [(blue, E, 2), (blue, E, 1)], [(green, W, 63), (green, W, 64)], [(orange, S,71), (orange, W, 72)]] [10,19,44,49,60,85,93,104,113,122]) render handleKeys update

{-
(Game
  [[Snake E blue],[Snake E blue],[Void],[Void],[Void],
  [Void],[Void],[Void],[Void],[Void],
  [Void],[Void],[Void],[Food],[Void],
  [Food],[Void],[Void],[Void],[Food],
  [Void],[Void],[Void],[Snake W red],[Snake W red]]
  [[(red, W, 24), (red, W, 25)], [(blue, E, 2), (blue, E, 1)]] [14,16,20])
-}

{-
(Game
 [[Snake E blue],[Snake E blue],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],
 [Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],
 [Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],
 [Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Snake W green],[Snake W green],
 [Void],[Void],[Void],[Void],[Void],[Void],[Snake S orange],[Snake W orange],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],
 [Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],
 [Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],
 [Food],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food],[Void],[Void],[Void],[Void],[Void],[Void],
 [Snake E red],[Snake E red],[Snake E red],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void]]
 [[(red, E, 131), (red, E, 130), (red, E, 129)], [(blue, E, 2), (blue, E, 1)], [(green, W, 63), (green, W, 64)], [(orange, S,71), (orange, W, 72)]] [10,19,44,49,60,85,93,104,113,122])
 -}



--------------------------------
-- MONSTER-SORTERING -- BEHÖVER FIXAS
--------------------------------

changeSort [] = []
changeSort (c:changes) = (changeSort lower) ++ c : (changeSort higher)
  where (lower, higher) = split' c changes [] []

split' p [] lower higher = (lower, higher)
split' p@(Erase dir1 c1 pos1) (c@(Erase dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(Erase dir1 c1 pos1) (c@(Write dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(Write dir1 c1 pos1) (c@(Erase dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(Write dir1 c1 pos1) (c@(Write dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher

split' p@(EraseFood pos1) (c@(EraseFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(EraseFood pos1) (c@(WriteFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(WriteFood pos1) (c@(EraseFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(WriteFood pos1) (c@(WriteFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher

split' p@(Erase dir1 c1 pos1) (c@(EraseFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(Erase dir1 c1 pos1) (c@(WriteFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(Write dir1 c1 pos1) (c@(EraseFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(Write dir1 c1 pos1) (c@(WriteFood pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher

split' p@(EraseFood pos1) (c@(Erase dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(EraseFood pos1) (c@(Write dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(WriteFood pos1) (c@(Erase dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
split' p@(WriteFood pos1) (c@(Write dir2 c2 pos2):changes) lower higher
  | pos1 <= pos2 = split' p changes lower (c:higher)
  | otherwise = split' p changes (c:lower) higher
