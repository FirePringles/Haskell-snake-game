module Main(main) where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

import Test.HUnit



-- The size of the grid
-- INVARIANT: Only positive integers
width, height, squareSide :: Float
width      = 16
height     = 9


-- The side of the squares in pixels
squareSide = 40


-- color of the grid
gridColor = makeColor 0.5 0.5 0.5 0.5


-- the maximum number of foods
foodIndex = 3


-- The actual size of the window, and translation constants x0, y0
pixelWidth = squareSide * width
x0 = (-pixelWidth/2)
pixelHeight = squareSide * height
y0 = (-pixelHeight/2)


-- The game window
window :: Display
window = InWindow "Snake" ((floor pixelWidth), (floor pixelHeight)) (0,0)



------------------------------------------------------------------------------
--DATA TYPES
------------------------------------------------------------------------------
{- SnakeGame 
   Represents a game state (Game board positions foodList updates) where
   board contains all the spaces of the board,
   positions represents all snakes in play,
   foodList the location of the food and
   updates is the number of updates since the current game began.

   The SnakeGame where board, positions and foodlist are empty represents the title screen
-}
data SnakeGame = Game Board [[Pos]] [Float] Float deriving (Show)



{- Pos
   Represents a piece of a snake (c, dir, pos), where
   c is the color of the snake,
   dir the direction the snake piece is headed and
   pos its position (the number of the square)on the board.
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
   The empty space is given by void.
-}
data Object = Snake Direction Color | Food | Void deriving (Eq, Show)



{- Direction
   Describes the direction of a Snake Object,
   where N means it is headed North, S south, W west, E east.
-}
data Direction = N | S | W | E deriving (Show, Eq)



{- Change
   Represents the instructions for changing a space on the board, either
   Writing a snake with a direction and a color to a space on the board,
   Erasing a snake with a direction and a color from a space on the board,
   Writing a piece of food to a space on the board or
   Erasing a piece of food from a space on the bord
-}
data Change = Write Direction Color Float | Erase Direction Color Float | WriteFood Float | EraseFood Float  deriving (Eq)



{- making Change an instance of Ord, for function <=
   Used for comparing changes, the number in each change is compared
-}
instance Ord Change where
 Write dir1 c1 f1 <= Write dir2 c2 f2 = f1 <= f2
 Write dir1 c1 f1 <= Erase dir2 c2 f2 = f1 <= f2
 Write dir1 c1 f1 <= WriteFood f2 = f1 <= f2
 Write dir1 c1 f1 <= EraseFood f2 = f1 <= f2

 Erase dir1 c1 f1 <= Write dir2 c2 f2 = f1 <= f2
 Erase dir1 c1 f1 <= Erase dir2 c2 f2 = f1 <= f2
 Erase dir1 c1 f1 <= WriteFood f2 = f1 <= f2
 Erase dir1 c1 f1 <= EraseFood f2 = f1 <= f2

 WriteFood f1 <= Write dir2 c2 f2 = f1 <= f2
 WriteFood f1 <= Erase dir2 c2 f2 = f1 <= f2
 WriteFood f1 <= WriteFood f2 = f1 <= f2
 WriteFood f1 <= EraseFood f2 = f1 <= f2

 EraseFood f1 <= Write dir2 c2 f2 = f1 <= f2
 EraseFood f1 <= Erase dir2 c2 f2 = f1 <= f2
 EraseFood f1 <= WriteFood f2 = f1 <= f2
 EraseFood f1 <= EraseFood f2 = f1 <= f2
------------------------------------------------------------------------------





-------------------------------------------------------------------------------
-- GRID
-------------------------------------------------------------------------------
{- vertiGrid i acc
   Creates the vertical lines of a grid with (width) number of squares along the x-axis
   RETURNS:(width-1) vertical lines, from top to bottom, spaced out evenly in the window
   EXAMPLES: vertiGrid 1 [] = [Line [(0, -40), (0, 40)]]   (on a 2x2 board, squareSide = 40)
-}
vertiGrid :: Float -> [Picture] -> [Picture]
-- VARIANT: width - i
vertiGrid i acc
 | i >= width = acc
 | otherwise =  vertiGrid (i+1) ((Line [(x, y0),(x, (-y0))]) : acc)
     where x = x0 + i*squareSide


{- horiGrid i acc
   Creates the horisontal lines of a grid with (height) number of squares along the y-axis
   RETURNS:(height-1) horisontal lines, from far left to far right, spaced out evenly in the window
   EXAMPLES: horiGrid 1 [] = [Line [(-40, 0), (40, 0)]]   (on a 2x2 board, squareSide = 40)
-}
horiGrid :: Float -> [Picture] -> [Picture]
-- VARIANT: height - i
horiGrid i acc
  | i >= height = acc
  | otherwise = horiGrid (i+1) ((Line [(x0, y),((-x0), y)]) : acc)
      where y = y0 + i*squareSide


{- grid
   Creates a grid of size (width x height), which fully covers the window
   RETURNS: The lines of a grid of size (width x height), where each square is of size (squareSide x squareSide)
   EXAMAMPLES: [color gridcolor [Line [(0, -40), (0, 40)]], color gridcolor [Line [(-40, 0), (40, 0)]]]   (on a 2x2 board, squareSide = 40)
-}
grid :: [Picture]
grid = map (color gridColor)(vertiGrid 1 (horiGrid 1 []))
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- RENDERING
------------------------------------------------------------------------------

{- render gs
   Creates a picture of the game state gs
   RETURNS: The rendering of all objects in gs
-}
render :: SnakeGame -> Picture
render (Game [] [] [] _) = pictures [translate (10 + x0) ((-100)-y0) (scale 0.5  0.5 (Text "S N A K E")), translate (10 + x0) ((-150)-y0)(scale 0.2 0.2 (Text "Press Enter"))]
render (Game board _ _ _) = pictures (renderBoard board (1,1) (grid))



{- renderBoard board (x,y) acc
   Creates pictures of all objects in board
   RETURNS: The rendering of all objects in board
-}
renderBoard :: Board -> (Float, Float) -> [Picture] -> [Picture]
-- VARIANT: length board
renderBoard [] _ acc = acc
renderBoard (s:spaces) (x,y) acc
  | x >= width = renderBoard spaces (1, (y+1)) (renderSpace s (x,y) acc)
  | otherwise = renderBoard spaces ((x+1), y) (renderSpace s (x,y) acc)



{- renderSpace s (x,y) acc
   Creates a picture of the first object in s, translates it into place and adds it to acc
   RETURNS: A rendering of the first object in s, followed by acc
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
   Updates gs by moving all snakes according to their directions and checking if they collide with food, the walls or themselves
   RETURNS: an updated game state
-}
moveSnakes :: Float -> SnakeGame -> SnakeGame
moveSnakes _ (Game _ [] _ _) = Game [] [] [] 0
moveSnakes _ (Game board pos foods (-1)) = Game board pos foods (-1)
moveSnakes s (Game board pos@(((c, d, p):snake1):positions) foods seconds)
  | c == red = Game(updateBoard eliminatedBoard 1 (changeSort changes)) newPositions newFoods (seconds+1)
  | otherwise = Game (updateBoard eliminatedAllBoard 1 (changeSort changesAll)) newPositionsAll newFoodsAll (seconds+1)
  
  where (updatedAiPos, updatedBoard) = updateComputerPos board positions foods []
        (eliminatedBoard, eliminatedPos) = eliminateCollidingSnakes updatedBoard (((c,d,p):snake1):updatedAiPos)
        (changes, newPositions, newFoods) = changeList eliminatedPos [] [] foods updatedBoard pos seconds

        (updatedAllPos, updatedAllBoard) = updateComputerPos board pos foods []
        (eliminatedAllBoard, eliminatedAllPos) = eliminateCollidingSnakes updatedAllBoard updatedAllPos
        (changesAll, newPositionsAll, newFoodsAll) = changeList eliminatedAllPos [] [] foods updatedAllBoard pos seconds



{- updateComputerPos board positions foods posAcc
   Calculates moves for all snakes in positions based on board and foods, and updated the direction of the snakes heads in positions and board
   RETURNS: Positions and board where the directions of the snake heads have been updated
   EXAMPLES: updateComputerPos [[Void], [Food], [Snake E red], [Snake E red]] [[(red, E, 4), (red, E, 3)]] [2] []
               = ([[(red, S, 4.0), (red, E, 3.0)]], [[Void],[Food],[Snake E red],[Snake S red]])           (on a 2x2 board)
-}
updateComputerPos :: Board -> [[Pos]] -> [Float] -> [[Pos]] -> ([[Pos]], Board)
-- VARIANT: length positions
updateComputerPos board [] foods posAcc = (posAcc, board)
updateComputerPos board (((c, dir, pos):p):positions) foods posAcc = updateComputerPos (changeDirSpace (c, newDir, pos) board 1) positions foods (((c, newDir, pos):p):posAcc)
  where newDir = computeAiMove board ((c, dir, pos):p) foods



{- updateBoard board pos changeList
   Updates the board according to the changes in changeList
   PRE: changeList is sorted so all changes have increasing positions
        no changes may result in a snake moving of the board
   RETURNS: board changed according to changeList
   EXAMPLES: updateBoard [[Void],[Void],[Snake S red],[Snake W red]] 1 [(Write S red 1), (Erase W red 4)] = [[Snake S Red, Void], [Void], [Snake S Red], [Void]]
             (on a 2x2 board)
-}
updateBoard :: Board -> Float -> [Change] -> Board
-- VARIANT: length changeList
updateBoard board i [] = board

updateBoard (s:spaces) i change@((Write dir c pos):changes)
  | pos < 1 = updateBoard (s:spaces) i changes
  | i /= pos = s: (updateBoard spaces (i+1)change)
updateBoard (s:spaces) i change@((Erase dir c pos):changes)
  | pos < 1 = updateBoard (s:spaces) i changes
  | i /= pos = s: (updateBoard spaces (i+1)change)
updateBoard (s:spaces) i change@((WriteFood pos):changes)
  | pos < 1 = updateBoard (s:spaces) i changes
  | i /= pos = s: (updateBoard spaces (i+1)change)
updateBoard (s:spaces) i change@((EraseFood pos):changes)
  | pos < 1 = updateBoard (s:spaces) i changes
  | i /= pos = s: (updateBoard spaces (i+1)change)
  
updateBoard (s:spaces) i ((Write dir c pos):changes) = updateBoard (((Snake dir c):s):spaces) (i) changes
updateBoard (s:spaces) i ((WriteFood pos):changes) = updateBoard (((Food):s):spaces) (i) changes
updateBoard (s:spaces) i ((Erase dir c pos):changes) = updateBoard ((eraseAux s (Erase dir c pos)):spaces) (i) changes
updateBoard (s:spaces) i ((EraseFood pos):changes) = updateBoard ((eraseAux s (EraseFood pos)):spaces) (i) changes

updateBoard board _ _ = board



{-eraseAux space eraseChange
  Erases the object in space specified by eraseChange
  PRE: The object specified by eraseChange is in space
  RETURNS: space without the object in eraseChange
  EXAMPLES: eraseAux [(Snake N blue),(Snake W red)] (Erase W red 12) = [Snake N blue, Void]
            eraseAux [(Snake, N, blue), Void] (Erase N Blue 12) = [Void]
-}
eraseAux :: Space -> Change -> Space
-- VARIANT: length space
eraseAux [] _ = []
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
{- changeList positions cAcc pAcc foodList board allPos updates
   Calculates all the changes to the board given by positions and foodList, and also gives a new list of positions and a new foodList
   RETURNS: The changes given by positions and foodList, the new positions, and the new foodList
   EXAMPLES: changeList [[(red, W, 13.0), (red, W, 14.0)]] [] [] [10,11] =
               = ([Write W red 12.0, Erase W red 14.0],[[(red, W, 12.0),(red, W, 13.0)]] [11.0, 10.0])
             changeList [[(red, W, 13.0), (red, W, 14.0)]] [] [] [10,13] =
	       = ([Write W red 12.0, EraseFood 13.0],[[(red, W, 12.0),(red, W, 13.0), (Red, W, 14.0)]] [10.0])   (on a 5x5 board)

-}
changeList :: [[Pos]] -> [Change] -> [[Pos]] -> [Float] -> Board -> [[Pos]] -> Float -> ([Change], [[Pos]], [Float])
-- VARIANT: length positions 
changeList [] changeAcc posAcc foods board allPos seconds = (changeAcc, posAcc, foods)
changeList (p:pos) changeAcc posAcc foods board allPos seconds = changeList pos (changes ++ changeAcc) (posAcc ++ [newPos]) newFoods board allPos seconds
  where (changes, newPos, newFoods) = translatePos p foods board allPos seconds



{- translatePos positions foodList board allPos updates
   Calculates what two changes of the board need to be performed considering positions and foodList, and also gives a new list of positions and foodList.
   RETURNS: The two changes of the board positions and foodList results in, the new list of positions and the new foodList
   EXAMPLES: translatePos [(red, W, 12), (red, W, 13)] [10, 5] = (Write W red 11.0 , Erase W red 13.0, [(red, W, 11.0), (red, W, 12.0)], [5.0, 10.0])
             translatePos [(red, W, 12), (red, W, 13)] [10, 12] = (Write W red 11.0 , EraseFood 12.0, [(red, W, 11.0), (red, W, 12.0), (red, W, 13.0)], [10.0])
             (on a 5x5 board)
-}
translatePos :: [Pos] -> [Float] -> Board -> [[Pos]] -> Float -> ([Change], [Pos], [Float])
translatePos (p:positions) foods board allPos seconds
  | foodCheck /= 0 = ([(Write newHeadDir c newHeadPos), (EraseFood oldTailPos), (WriteFood newFoodCoord)], ((c, newHeadDir, newHeadPos):newTail), newFoods)
  | otherwise = ([(Write newHeadDir c newHeadPos), (Erase oldTailDir c oldTailPos)], ((c, newHeadDir, newHeadPos):newTail), newFoods)
  where ((c, newHeadDir, newHeadPos), newFoods, foodCheck, newFoodCoord) = translatePosHead p foods board allPos seconds 
        (newTail, (oldTailDir, oldTailPos)) = translatePosTail [] (p:positions) foodCheck



{- translatePosTail acc positions foodCheck
   Computes a new tail from positions, and also gives what should be erased, the last piece of the tail if foodCheck == 0, or the food in position foodCheck otherwise
   PRE: positions contain at least two positions
   RETURNS: A new tail, and the coordinates of a tail to be erased, or a food coordinate.
   EXAMPLES: translatePosTail [] [(red, W, 12), (red, W, 13)] 0 = ([(red, W, 12.0)], (W, 13.0))
             translatePosTail [] [(red, W, 12), (red, W, 13)] 5 = ([(red, W, 12.0), (red, W, 13.0)], (N, 5.0))     (on a 5x5 board)
-}
translatePosTail :: [Pos] -> [Pos] -> Float -> ([Pos], (Direction, Float))
-- VARIANT: length positions
translatePosTail acc [(c1, dir1, pos1), (c2, dir2, pos2)] foodCheck
  | foodCheck == 0 = ((reverse ((c1, dir1, pos1):acc)), (dir2, pos2))
  | otherwise = ((reverse ((c2, dir2, pos2):(c1, dir1, pos1):acc)), (N, foodCheck))
translatePosTail acc (p:positions) foodCheck = translatePosTail (p:acc) positions foodCheck



{- translatePosHead head foodList board updates positions
   Creates a new position for head based on its direction and checks if the head is in contact with food.
   RETURNS: A new position for head, a foodList where food in the same position as the head is removed, and the position of eventual removed food.
   EXAMPLES: translatePosHead (red, W, 14) [10,11] = ((red, W, 13.0), [11.0,10.0], 0.0)     
             translatePosHead (red, W, 14) [10, 14] = ((red, W, 13.0), [10.0], 14.0)       (on a 5x5 board)

-}
translatePosHead :: Pos -> [Float] -> Board -> [[Pos]] -> Float-> (Pos, [Float], Float, Float)
translatePosHead (c, dir, pos) foods board  positions seconds 
  | dir == N = ((c, dir, pos+width), newFoods, tailUpdate, newFoodPos)
  | dir == S = ((c, dir, pos-width), newFoods, tailUpdate, newFoodPos)
  | dir == W = ((c, dir, pos-1), newFoods, tailUpdate, newFoodPos)
  | dir == E = ((c, dir, pos+1), newFoods, tailUpdate, newFoodPos)
    where (newFoods, tailUpdate, newFoodPos) = checkForFood pos foods [] board positions seconds



{- checkForFood pos foodList acc board positions seconds
   Checks if pos is in foodList, and if so returns foodList without pos, paired with pos, otherwise the same foodList paired with 0 is returned.
   RETURNS: (foodList without pos, pos) if pos in foodList, otherwise (foodList, 0). The order of foodList is not preserved.
   EXAMPLES: checkForFood 4 [1,2,3] [] = ([3.0, 2.0, 1.0], 0.0)
             checkForFood 4 [3,4,5] [] = ([3.0, 5.0], 4.0)
-}
checkForFood :: Float -> [Float] -> [Float] -> Board -> [[Pos]] -> Float -> ([Float], Float, Float)
-- VARIANT: length foodList
checkForFood _ [] acc _ _ _ = (acc, 0, 0)
checkForFood headPos (f:foods) acc board positions seconds
  | headPos == f = ((acc ++ newFoods),f, newFoodPos)
  | otherwise = checkForFood headPos foods (f:acc) board positions seconds
    where (newFoods, newFoodPos) = spawnNew foods board positions seconds



{- spawnNew foodList board pos sec
Adds a new food obejct at a random position into the foodlist if the amount of food in foodlist is lower then a global variable
PRE: The foodList must accord with the boards food Values to get a useful result
RETURNS: A tuple containing the new foodlist and a float value of the new foods position
EXAMPLES: spawnNew [1.0,2.0] [[Food],[Food],[Void],[Void]] [[(red,W,3.0)]] 1.0 = ([4.0,1.0,2.0],4.0)
spawnNew [2.0,4.0] [[Void],[Food],[Void],[Food],[Void],[Void]] [[(red,W,3.0)]] 1.0 = ([3.0,2.0,4.0],3.0)
-}
spawnNew :: [Float] -> Board -> [[Pos]] -> Float -> ([Float], Float)
spawnNew ys board positions seconds
  | (length ys < foodIndex) && (emptysLength > 0) =  ((randomPos:ys), randomPos)
  | otherwise = (ys, 0)
    where (emptys, emptysLength) = emptySpaces board
          randomNum = randomNumber seconds positions
          randomIndex = mod (round randomNum) (round emptysLength)
          randomPos = emptys !! randomIndex



{- emptySpaces board
Checks all positions on a board to see which of them are empty
RETURNS: A list of every position from board containging Void. It also returns a float number showing how many there are-
EXAMPLES:emptySpaces [[Void],[Food],[Void]] = ([1.0,3.0],2.0)
emptySpaces [[Food],[Void],[Food]] = ([2.0],1.0)
-}
emptySpaces :: Board -> ([Float], Float)
emptySpaces [] = ([], 0)
emptySpaces (x:xs) = (z, w) where
  z = emptySpacesAux (x:xs) 1.0
  w = fromIntegral (length z)



{-emptySpacesAux board x
Goes through a board and checks each objects if's a void. If it is then it marks that position and puts it into a list
PRE: the x value should always start at 1.0 to get a clean result 
RETURNS: a list containing every position on the board containing Void
EXAMPLE: emptySpacesAUX [[Void][Void][Food]] 1.0 = [1.0,2.0]
emptySpacesAux [[Food],[Food],[Void]] 1.0 = [3.0]
-}
--Variant length xs
emptySpacesAux :: Board -> Float -> [Float]
emptySpacesAux [] _ = []
emptySpacesAux (x:xs) y
  | x == [Void] = y:emptySpacesAux xs (y+1.0)
  | otherwise = emptySpacesAux xs (y+1.0)



{- randomNumber x pos
Calculates a number based on the x value and the length of the first element in pos
RETURNS: A float based on the x and pos elements
EXAMPLE: randomNumber 1.0 [[(red,W,3.0)]] = 1.0
randomNumber 2.0 [[(red,W,3.0)]] = 4.0
randomNumber 2.0 [[(red,W,3.0),(red,W,4.0)]] = 8.0
-}
randomNumber :: Float -> [[Pos]] -> Float
randomNumber seconds pos = fromIntegral (round (seconds^2)*(length(pos !! 0)))



{- changeSort changeList
   Quicksort algorithm for changeList
   RETURNS: changeList sorted by the number in each change
   EXAMPLES: changeSort [EraseFood 7, WriteFood 2, Write N red 22, Erase S blue 5] = [WriteFood 2.0, Erase S blue 5.0, EraseFood 7.0, Write N red 22] 
-}
changeSort :: [Change] -> [Change]
-- VARIANT: length changeList
changeSort [] = []
changeSort (c:changes) = (changeSort lower) ++ c : (changeSort higher)
  where (lower, higher) = changeSplit c changes [] []



{- changeSplit c changeList lower higher
   Splits changeList into a list of all changes < c and a list of all changes  >= c
   RETURNS: All changes < c and all changes >= c, in the same order as changeList
   EXAMPLES: changeSplit (EraseFood 7) [WriteFood 2, Write N green 7, Write N red 22, Erase S blue 5] [] []
               = ([Erase S blue 5.0, WriteFood 2.0], [Write N red 22.0, Write N green 7.0])
-}
changeSplit :: Change -> [Change] -> [Change] -> [Change] -> ([Change],[Change])
-- VARIANT: length changeList
changeSplit c [] lower higher = (lower, higher)
changeSplit c1 (c2:changes) lower higher
  | c1 <= c2 = changeSplit c1 changes lower (c2:higher)
  | otherwise = changeSplit c1 changes (c2:lower) higher

-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
--COLLISION DETECTION
-------------------------------------------------------------------------------

{- eliminateCollidingSnakes board positions
   Removes all snakes whose head is colliding with a snake, or leaving the board, from board and positions
   RETURNS: board and positions without all snakes whose head is colliding with a snake or leaving the board
   EXAMPLES: eliminateCollidingSnakes [[Snake S red, Snake W blue],[Snake W blue],                [Void],
                                       [Snake S red],              [Snake E blue],                [Snake N orange],
                                       [Void],                     [Snake W green],               [Snake W green, Snake N orange]]
                                      [[(red, S, 1), (red, S, 4)], [(blue, W, 1), (blue, W, 2)],
               	                       [(green, W, 8), (green, W, 9)], [(orange, N, 9), (orange, N, 8)]]
				     = [[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake W green],[Snake W green]]
                                      [[(green, W, 8), (green, W, 9)]]                                   (on a 3x3 board)
-}
eliminateCollidingSnakes :: Board -> [[Pos]] -> (Board, [[Pos]])
eliminateCollidingSnakes board pos = eliminateSnakes board pos colorList
  where collisionList = deadSnakes board pos
        colorList = (snakeOutsideBoard (head pos)) ++ collisionList



{- snakeOutsideBoard positions
   Checks if the head of the snake given by positions has left the board, and returns its color if it has 
   RETURNS: The color of the snake given by positions if it has left the board, otherwise []
   EXAMPLES: snakeOutsideBoard [(red, S, (-4)), (red, S, 1)] = [red]
             snakeOutsideBoard [(red, W, 5), (red, W, 6)] = [red]
             snakeOutsideBoard [(red, E, 12), (red, E, 13)] = [red]
-}
snakeOutsideBoard :: [Pos] -> [Color]
snakeOutsideBoard ((c1, dir1, pos1):(c2, dir2, pos2):positions)
  | (pos1 < 1) || (pos1 > (width*height)) || ((modPos1 == 0) && (modPos2 == 1)) || ((modPos1 == 1) && (modPos2 == 0)) = [c1]
  | otherwise = []
    where intWidth = floor width
          modPos1 = mod (floor pos1) intWidth
          modPos2 = mod (floor pos2) intWidth



{- eliminateSnakes board positions c
   Removes all snakes of a color in c from board and positions
   RETURNS: board and positions without snakes of a color in c
   EXAMPLES: eliminateSnakes [[Snake W red, Snake N green],[Snake W red, Snake S blue],[Snake N green],[Snake S blue]]
                             [[(red, W, 2), (red, W, 1)],[(blue, S, 2), (blue, S, 4)],[(green, N, 3), (green, N, 1)]]
                             [red, blue]
			     = [[Snake N green, Void],[Void],[Snake N green, Void],[Void]] [[[(green, N, 3), (green, N, 1)]]
-}
eliminateSnakes :: Board -> [[Pos]] -> [Color] -> (Board, [[Pos]])
-- VARIANT: length c
eliminateSnakes board pos [] = (board, pos)
eliminateSnakes board pos (c:colors) = eliminateSnakes newBoard newPos colors
  where
    newBoard = map (eliminateFromSpace c) board
    newPos = eliminatePos c pos



{- eliminateFromSpace c space
   Removes all snakes with color c from space
   RETURNS: space without eventual snakes with color c
   EXAMPLES: eliminateFromSpace blue [Snake N red, Snake E blue, Void] = [Snake N red, Void]
             eliminateFromSpace green [Snake N red, Snake E blue, Void] = [Snake N red, Snake E blue, Void] 
-}
eliminateFromSpace:: Color -> Space -> Space
-- VARIANT: length space
eliminateFromSpace _ [] = [Void]
eliminateFromSpace _ [Void] = [Void]
eliminateFromSpace c2 ((Snake dir c1):objects) 
  | c1 == c2 = eliminateFromSpace c2 objects
  | otherwise = (Snake dir c1): eliminateFromSpace c2 objects 
eliminateFromSpace c (o:objects)= o: eliminateFromSpace c objects



{- eliminatePos c pos
   Eliminates the list of positions with color c from pos
   RETURNS: pos without the list with positions of color c
   EXAMPLES: eliminatePos blue [[(red, W, 22), (red, W, 23)],[(blue, N, 8),(blue, N, 3)],[(green, S, 10), (green, S, 15)]]
                = [[(red, W, 22), (red, W, 23)], [(green, S, 10), (green, S, 15)]]
-}
eliminatePos :: Color -> [[Pos]] -> [[Pos]]
-- VARIANT: length pos
eliminatePos _ [] = []
eliminatePos c1 (((c2, dir, pos):snake):positions)
  | c1 == c2 = positions
  | otherwise = ((c2, dir, pos):snake): eliminatePos c1 positions



{- deadSnakes board positions
   Checks if the heads of all snakes given by positions are crashing into a snake, and returns the colors of all that do
   RETURNS: The colors of all snakes given by positions that are crashing into a snake
   EXAMPLES: deadSnakes [[Snake E red], [Snake E red, Snake N blue], [Void], [Snake W blue]] [[(red, E, 2), (red, E, 1)], [(blue, W, 4), (blue, N, 2)]] = [red]
             deadSnakes [[Snake E red], [Snake E red], [Snake W blue], [Snake W blue]] [[(red, E, 2), (red, E, 1)], [(blue, W, 4), (blue, W, 3)]] = []
	     (on a 2x2 board)

-}
deadSnakes :: Board -> [[Pos]] -> [Color]
-- VARIANT: length positions
deadSnakes board [] = []
deadSnakes board (((c, dir, pos):snake1):positions)
  | (pos <= (width*height) && (pos >= 1)) && (snakeCollision (board !! ((floor pos)-1)) c 0) = c: deadSnakes board positions
  | otherwise = deadSnakes board positions



{- snakeCollision space c count
   Checks if there are any snakes of a different color from c, or multiple snakes of color c, in a space
   RETURNS: True if there is a snake whose color /= c, or if there are multiple snakes whose color == c, in space, otherwise False
   EXAMPLES: snakeCollision [Snake N red, Food, Void] red 0 = False
             snakeCollision [Snake N red, Snake S red, Void] red 0 = True
             snakeCollision [Snake N red, Snake S blue, Void] red 0 = True

-}
snakeCollision :: Space -> Color -> Int -> Bool
-- VARIANT: length space
snakeCollision [] _ _ = False
snakeCollision ((Snake dir c1):objects) c2 i
  | c1 /= c2 = True
  | c1 == c2 && i == 0 = snakeCollision objects c2 1
  | c1 == c2 && i == 1 = True
  | otherwise = snakeCollision objects c2 i
snakeCollision (o:objects) c i = snakeCollision objects c i
--------------------------------------------------------------------------------





-------------------------------------------------------------------------------
-- AI
-------------------------------------------------------------------------------

{- computeAiMove board positions foods
   Computes the "best" direction for the snake given by positions for reaching the closest food, taking the board into consideration
   RETURNS: The best direction for the snake given by positions for reaching the closest food, avoiding crashing into snakes if possible
   EXAMPLES: computeAiMove [[Snake N red],[Void],        [Void],
                            [Snake N red],[Snake E blue],[Void],
                            [Void],       [Snake S blue],[Food]]
                           [(red, N, 4), (red, N, 1)] [9]        = N     (on a 3x3 board)
-}
computeAiMove :: Board -> [Pos] -> [Float] -> Direction
computeAiMove board positions@((c1, dir1, pos1):(c2, dir2, pos2):pos) foods = finalDir
  where (dirPriority, headx, heady) = directionPriority (closestFood (convertToCoord pos1) 999 foods) pos1
        borderx = illegalx headx
        bordery = illegaly heady
        backwards = illegalBackward positions
        preBoardCheck = removeIllegalDir bordery (removeIllegalDir borderx (removeIllegalDir backwards dirPriority))
        finalDir = finalRemoval preBoardCheck (checkBoard board (removeOutOfBounds [(pos1-width, S), (pos1-1, W), (pos1+1, E), (pos1+width, N)]) 1)



{- removeOutOfBounds posDirs
   Remove all elements of posDirs that have positions outside the board
   RETURNS: posDirs without all elements that contain positions < 1 or > width*height
   EXAMPLES: removeOutOfBounds [((-2), S), (2, W), (4, E), (8, N)] = [(2.0, W), (4.0, E), (8.0, N)]
             removeOutOfBounds [(20, S), (23, W), (25, E), (29, N)] = [(20.0, S), (23.0, W), (25.0, E)]     (on a 5x5 board)
-}
removeOutOfBounds :: [(Float, Direction)] -> [(Float, Direction)]
-- VARIANT: length posDirs
removeOutOfBounds [] = []
removeOutOfBounds ((pos, dir):dirs)
  | (pos < 1) || (pos > (width*height)) = removeOutOfBounds dirs
  | otherwise = (pos, dir) : removeOutOfBounds dirs



{- closestFood headCoord currentClosest foods
   Calculates which piece of food is the closest to headCoord. (headCoord given in (x,y)-coordinates and foods in board positions)
   RETURNS: The position of the food closest to headCoord
   EXAMPLES: closestFood (2,3) 999 [4, 20] = 4.0     (on a 5x5 board)
-}
closestFood :: (Float, Float) -> Float -> [Float] -> Float
-- VARIANT: length foods
closestFood _ closest []  = closest
closestFood (headx, heady) closest (f:foods)
  | (sqrt((closestx - headx)^2 + (closesty - heady)^2)) <= (sqrt((foodx - headx)^2 + (foody - heady)^2)) = closestFood (headx, heady) closest foods
  | otherwise = closestFood (headx, heady) f foods
  where (closestx, closesty) = convertToCoord closest
        (foodx, foody) = convertToCoord f



{- finalRemoval dirs illegals
   Gives the first direction of dirs without all directions in illegals,
     unless illegals contain all directions in dirs, in which case the last remaining one in dirs is returned
   RETURNS The first direction of dirs without all elements in illegals, or the last to be deleted if illegals contain all directions of dirs
   EXAMPLES: finalRemoval [N,S,E] [N] = S
             finalRemoval [N,S,E] [N,S,E] = E
             finalRemoval [N,S,E] [E,S,N] = N 
-}
finalRemoval :: [Direction] -> [Direction] -> Direction
-- VARIANT: length illegals
finalRemoval (d:directions) [] = d
finalRemoval [d] _ = d
finalRemoval directions (i:illegals) = finalRemoval (removeIllegalDir [i] directions) illegals



{- checkBoard board posDirs  i
   Checks if there are any snakes on each of the positions of posDirs on the board, and if so returns the paired Direction.
   PRE: The positions in posDirs are >= 1, <= width*height
   RETURNS: The directions paired with positions on the board that contain snakes.
   EXAMPLES: checkBoard [[Void], [Snake W red], [Void], [Snake E blue]] [(1, W), (4, N)] 1 = [N]
             checkBoard [[Void], [Snake W red], [Void], [Food]] [(1, W), (4, N)] 1 = []             (on a 2x2 board)
-}
checkBoard :: Board -> [(Float, Direction)] -> Float -> [Direction]
-- VARIANT: length Board * length posDirs
checkBoard [] _ _ = []
checkBoard _ [] _ = []
checkBoard (s:spaces) coordinates@((c, dir):coord) i
  | (i == c) && (checkForSnakes s) = dir : (checkBoard spaces coord (i+1))
  | (i == c) = checkBoard spaces coord (i+1)
  | otherwise = checkBoard spaces coordinates (i+1)



{- checkForSnakes space
   Checks if there are any snakes in space
   RETURNS: True if space contains at least one snake, otherwise False
   EXAMPLES: checkForSnakes [Food, Void] = False
             checkForSnakes [Snake W red] = True
-}
checkForSnakes :: Space -> Bool
-- VARIANT: length space
checkForSnakes [] = False
checkForSnakes ((Snake _ _):spaces) = True
checkForSnakes (s:spaces) = checkForSnakes spaces



{- removeIllegalDir illegalDir inputDirs
   Removes the direction in illegalDir from inputDirs, if it exists in inputDirs
   PRE: inputDirs does not contain duplicate directions
        illegalDir contains at most one direction
   RETURNS: inputDirs without the direction in illegalDir, if it exists in inputDirs
   EXAMPLES: removeIllegalDir [N] [S,N,W] = [S,W]
             removeIllegalDir [N] [S,E,W] = [S,E,W]
             removeIllegalDir [] [S,E,W] = [S,E,W]
-}
removeIllegalDir :: [Direction] -> [Direction] -> [Direction]
-- VARIANT: length illegalDir * length inputDirs
removeIllegalDir [] directions = directions
removeIllegalDir _ [] = []
removeIllegalDir (i:illegal) (d:directions)
  | i == d = directions
  | otherwise = d: (removeIllegalDir (i:illegal) directions)



{- illegalBackward pos
   Gives the direction that points from the first element of pos to the second, which is an illegal direction for the first
   RETURNS: the opposite direction of the second element of pos
   EXAMPLES: illegalBackward [(red, N, 5), (red, W, 4)] = [E]
-}
illegalBackward :: [Pos] -> [Direction]
illegalBackward (p1:(c, dir, pos):positions)
  | dir == N = [S]
  | dir == S = [N]
  | dir == E = [W]
  | dir == W = [E]



{- illegaly heady
   Checks if going North or South from heady will move off the board, and if so returns the illegal direction
   PRE: height of board >= 2
   RETURNS: [N], respectively [S] if going W, respectively E from heady will move off the board. Otherwise []
   EXAMPLES: illegaly 1 = [S]
             illegaly 3 = []
             illegaly 5 = [N]      (on a 5x5 board)
-}
illegaly :: Float -> [Direction]
illegaly heady
  | heady >= height = [N]
  | heady <= 1 = [S]
  | otherwise = []



{- illegalx headx
   Checks if going West or East from headx will move off the board, and if so returns the illegal direction.
   PRE: width of board >= 2
   RETURNS: [W], respectively [E] if going W, respectively E from headx will move off the board. Otherwise []
   EXAMPLES: illegalx 6 = [W]
             illegalx 7 = []
             illegalx 10 = [E]    (on a 5x5 board)
-}
illegalx :: Float -> [Direction]
illegalx headx
  | modWidth == 1 = [W]
  | modWidth == 0 = [E]
  | otherwise = []
  where modWidth =  mod (floor headx) (floor width)



{- directionPriority food headPos
   Creates a priority list of directions in order for the snake head on headPos to reach food, and also converts headPos to (x,y)-coordinates
   RETURNS: A list of directions, with decreasing priority, for the snake on headPos to reach food, and the (x,y) coordinates corresponding to headPos
   EXAMPLES: directionPriority 4 15 = ([S,W,E,N], 5.0, 3.0) (on a 5x5 board)
-}
directionPriority :: Float -> Float -> ([Direction], Float, Float)
directionPriority food head
  | (abs xDistance) >= (abs yDistance) = ((priorityList "x" xDistance yDistance), headx, heady)
  | otherwise = ((priorityList "y" xDistance yDistance), headx, heady)
  where (foodx, foody) = convertToCoord food
        (headx, heady) = convertToCoord head
        (xDistance, yDistance) = ((foodx - headx), (foody - heady))



{- priorityList direction xDistance yDistance
   Makes a list of directions, sorted by which will bring the snake closest to the point described by xDistance and yDistance
   RETURNS: A list of directions with decreasing priority
   EXAMPLES: priorityList "x" (-4) 3 = [W,N,S,E]
             priorityList "y" 3 4 = [N,E,W,S]       (on a 5x5 board)
-}
priorityList d x y
  | (d == "x") && (x <= 0) && (y <= 0) = [W, S, N, E]
  | (d == "x") && (x <= 0) && (y > 0) = [W, N, S, E]
  | (d == "x") && (x > 0) && (y <= 0) = [E, S, N, W]
  | (d == "x") && (x > 0) && (y > 0) = [E, N, S, W]
  | (d == "y") && (x <= 0) && (y <= 0) = [S, W, E, N]
  | (d == "y") && (x <= 0) && (y > 0) = [N, W, E, S]
  | (d == "y") && (x > 0) && (y <= 0) = [S, E, W, N]
  | (d == "y") && (x > 0) && (y > 0) = [N, E, W, S]



{- convertToCoord pos
   Converts pos from representing a space by its index on the board, to the coordinates of the board seen as a plane.
   RETURNS: The (x,y)-coordinates corresponding to pos
   EXAMPLES: convertToCoord 1 = (1.0, 1.0)
             convertToCoord 14 = (4.0, 3.0)        (on a 5x5 board)
-}
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
-- The idea of handleKeys was given by http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
handleKeys :: Event -> SnakeGame -> SnakeGame
handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) (Game [] [] [] _) = initialGamestate
handleKeys (EventKey (SpecialKey KeyEnter) _ _ _) _ = initialGamestate

handleKeys (EventKey (Char 'q') _ _ _) (Game board pos foods (-1)) = Game board pos foods 1
handleKeys (EventKey (Char 'e') _ _ _) (Game board pos foods s) = Game board pos foods (-1)


handleKeys (EventKey (Char 'w') _ _ _) (Game board (((c1, dir1, square1):(c2, dir2, square2):pos):positions) foods s)
  | dir2 /= S = Game (changeDirSpace (c1, N, square1) board 1) (((c1, N, square1):(c2, dir2, square2):pos):positions) foods s
  
handleKeys (EventKey (Char 'a') _ _ _) (Game board (((c1, dir1, square1):(c2, dir2, square2):pos):positions) foods s)
  | dir2 /= E = Game (changeDirSpace (c1, W, square1) board 1) (((c1, W, square1):(c2, dir2, square2):pos):positions) foods s
  
handleKeys (EventKey (Char 's') _ _ _) (Game board (((c1, dir1, square1):(c2, dir2, square2):pos):positions) foods s)
  | dir2 /= N = Game (changeDirSpace (c1, S, square1) board 1) (((c1, S, square1):(c2, dir2, square2):pos):positions) foods s
  
handleKeys (EventKey (Char 'd') _ _ _) (Game board (((c1, dir1, square1):(c2, dir2, square2):pos):positions) foods s)
  | dir2 /= W = Game (changeDirSpace (c1, E, square1) board 1) (((c1, E, square1):(c2, dir2, square2):pos):positions) foods s
handleKeys _ game = game



{- changeDirSpace pos board i
   Changes the direction of snake head pos on board
   RETURNS: board, but the snake head given by pos has a direction given by pos
   EXAMPLES: changeDirSpace (red, E, 1) [[Snake S red],[Void],[Snake S red],[Void]] 1 = [[Snake E red],[Void],[Snake S red],[Void]]     (on a 2x2 board)
-}
changeDirSpace :: Pos -> Board -> Float -> Board
-- VARIANT: square - i
changeDirSpace (c, dir, square) (s:spaces) i
  | (square > (width*height)) || (square < 1) = (s:spaces)
  | i == square = (changeDirSpaceAux (c, dir, square) s):spaces
  | otherwise = s:(changeDirSpace (c, dir, square) spaces (i+1))



{- changeDirSpaceAux pos objects
   Steps through objects and changes the snake with the same color as pos to pos
   PRE: A snake of color given by pos actually exists in objects
   REURNS: objects, where the snake with the same color as pos is changed to pos
   EXAMPLES: changeDirSpaceAux (red, N, 13) [(Snake S blue), (Snake W red)] = [Snake S blue, Snake N red]
-}
changeDirSpaceAux :: Pos -> Space -> Space
-- VARIANT: length objects
changeDirSpaceAux (c1, dir1, square) ((Snake dir2 c2):objects)
  | c1 == c2 = (Snake dir1 c1):objects
changeDirSpaceAux pos (o:objects) = o:(changeDirSpaceAux pos objects)
------------------------------------------------------------------------------




------------------------------------------------------------------------------
-- INITIAL GAME STATE
------------------------------------------------------------------------------

{-initialGamestate
creates an initial game state of type SnakeGame.
RETURNS: a SnakeGame depending on the size of the board and the number of choosen snakes.
PRE: TRUE
EXAMPLES: initialGamestate = Game [[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake E (RGBA 0.0 0.0 1.0 1.0)],[Snake N (RGBA 0.0 0.0 1.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food,Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food,Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food,Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake S (RGBA 0.0 1.0 0.0 1.0)],[Snake W (RGBA 0.0 1.0 0.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake W (RGBA 1.0 0.5 0.0 1.0)],[Snake W (RGBA 1.0 0.5 0.0 1.0)]] [[(RGBA 1.0 0.0 0.0 1.0,E,2.0),(RGBA 1.0 0.0 0.0 1.0,E,1.0)],[(RGBA 0.0 0.0 1.0 1.0,N,16.0),(RGBA 0.0 0.0 1.0 1.0,E,15.0)],[(RGBA 0.0 1.0 0.0 1.0,S,129.0),(RGBA 0.0 1.0 0.0 1.0,W,130.0)],[(RGBA 1.0 0.5 0.0 1.0,W,143.0),(RGBA 1.0 0.5 0.0 1.0,W,144.0)]] [54.0,90.0,72.0] 0.0
-}
initialGamestate = Game boardWithFood pos [food1, food2, food3]  0
  where
    (board, pos) = initialAux (genFromSize width height) [] 3 0
    middle = fromIntegral (round (width*height / 2))
    food1 = middle - width -2
    food2 = middle + width + 2
    food3 = middle
    boardWithFood = writeFood board [food1,food3, food2] 1



{-writeFood board [pos] i
inserts Food pieces into the board
RETURNS: board with Food inserted in desired pos
PRE: [Pos] should be sorted.
EXAMPLES:
writeFood (genFromSize 2 2) [1,2] 1 =
[[Food,Void],[Food,Void],[Void],[Void]]
writeFood (genFromSize 2 2) [1] 1 =
[[Food,Void],[Void],[Void],[Void]]
writeFood (genFromSize width height) [] 1 =
[[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void]]
-}    
writeFood :: Board -> [Float] -> Float -> Board
writeFood board [] _ = board
writeFood [] _ _ = []
writeFood (s:spaces) (p:positions) i
  | i == p = (Food:s) : writeFood spaces positions (i+1)
  | otherwise = s: writeFood spaces (p:positions) (i+1)



{-initialAux b pos snakes actualsnakes
A function that returns a pair with the desired board (but without food pieces) and positions.
RETURNS: a pair consisting a board with inserted snakes and the positions
PRE: Snakes <= 4
EXAMPLES: initialAux (genFromSize width height) [] 4 0 =
([[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake E (RGBA 0.0 0.0 1.0 1.0)],[Snake N (RGBA 0.0 0.0 1.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake S (RGBA 0.0 1.0 0.0 1.0)],[Snake W (RGBA 0.0 1.0 0.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake W (RGBA 1.0 0.5 0.0 1.0)],[Snake W (RGBA 1.0 0.5 0.0 1.0)]],[[(RGBA 1.0 0.0 0.0 1.0,E,2.0),(RGBA 1.0 0.0 0.0 1.0,E,1.0)],[(RGBA 0.0 0.0 1.0 1.0,N,16.0),(RGBA 0.0 0.0 1.0 1.0,E,15.0)],[(RGBA 0.0 1.0 0.0 1.0,S,129.0),(RGBA 0.0 1.0 0.0 1.0,W,130.0)],[(RGBA 1.0 0.5 0.0 1.0,W,143.0),(RGBA 1.0 0.5 0.0 1.0,W,144.0)]])
-}
initialAux :: Board -> [[Pos]] -> Int -> Int -> (Board, [[Pos]])
initialAux b@(s:spaces) pos snakes actualsnakes
  | snakes == actualsnakes = (b,pos)
  | otherwise = initialAux (oldspaces ++ newboard) newpos snakes (actualsnakes+1) 
    where   
      (newspaces,oldspaces) = goTo b (actualsnakes+1) 1 []
      (newboard, newpos) = insertSnake newspaces pos (actualsnakes+1)



{-genFromSize w h
creates a Board with only Voids
RETURNS: returns a list containing lists of Voids depending on the value of w and h.
PRE: Non negative numbers
EXAMPLES:
genFromSize width height = [[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void]]
genFromSize 1 0 = []
genFromSize 0 1 = []
genFromSize 1 1 = [[Void]]
-}      
genFromSize :: Float -> Float -> Board
genFromSize wid hig = replicate ((floor wid) * (floor hig)) [Void]



{-insertSnake b pos corner
takes care of cases when corners are empty (Void)  and inserts a snake when it is. 
RETURNS: b and pos where Snakes have been inserted in corner
PRE: TRUE
EXAMPLES:
insertSnake (genFromSize 2 2) [] 1 =
([[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Void],[Void]],[[(RGBA 1.0 0.0 0.0 1.0,E,2.0),(RGBA 1.0 0.0 0.0 1.0,E,1.0)]])
-}
--insertSnake :: Board -> [[Pos]] -> Int -> (Board, [[Pos]])
insertSnake (s:s1:spaces) pos corner
  | s == [Void] && corner == 1 = ([(Snake E red)]:[(Snake E red)]:spaces, pos ++ [[(red, E, 2), (red,E,1)]])
  | s == [Void] && corner == 2 = ([(Snake E blue)]:[(Snake N blue)]:spaces, pos ++ [[(blue, N, width), (blue, E, (width - 1)) ]])
  | s == [Void] && corner == 3 = ([(Snake S green)]:[(Snake W green)]:spaces, pos ++ [[(green, S, (((height-1)* width)+1)), (green, W, ((height-1) * width)+2)]])
  | s == [Void] && corner == 4 = ([(Snake W orange)]:[(Snake W orange)]:spaces, pos ++ [[(orange, W, ((width*height)-1)), (orange, W, (width*height))]])



{-goTo b corner index b1
keeps track of all the corners and the spaces that has been stepped through
RETURNS: a pair of board where the first board is the board containing all corners and the second is the rest of the board.
PRE: Non-negative corners and index. non empty b.
EXAMPLES:
goTo 
goTo (genFromSize 1 1) 1 1 [] =
([[Void]],[])
goTo (genFromSize width height) 1 1 [] =
([[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void]],[])
-}
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




-- The game loop
main = play window white 3 (Game [] [] [] 0) render handleKeys moveSnakes



-- Run tests that work on any dimensions
-- main = runTests1

-- Run tests that need width = height = 2, change values at top of document before running these
-- main = runTests2

-- Tests of render/initialState
-- main = display window white renderTest1
-- main = display window white renderTest2

---------------------------------------------------------------------------
-- TEST CASES
---------------------------------------------------------------------------

-- test cases that works no matter dimensions of the board
runTests1 = runTestTT anyDimTests

-- test cases that need an input board, 2x2 is chosen for tests
runTests2 = runTestTT twoXtwoTests

anyDimTests = TestList [vertiGridTest, horiGridTest]
twoXtwoTests = TestList [vertiGridTest, horiGridTest, computeAiMoveTest, spawnNewTest, selfCollisionTest, changeDirNegativeTest, changeDirOutsideTest]


-- Checks if the first and last vertical lines are placed correctly
vertiGridTest = TestCase (assertEqual "vertiGridTest" ((Line [(x2, y0), (x2, -y0)]), (Line [(x1, y0), (x1, -y0)])) (head (vertiGrid 1 []), last (vertiGrid 1 [])))
  where x1 = x0 + squareSide
        x2 = x0 + squareSide*(width-1)


-- Checks if the first and last vertical lines are placed correctly
horiGridTest = TestCase (assertEqual "horiGridTest" ((Line [(x0, y2), (-x0, y2)]), (Line [(x0, y1), (-x0, y1)])) (head (horiGrid 1 []), last (horiGrid 1 [])))
  where y1 = y0 + squareSide
        y2 = y0 + squareSide*(height-1)


-- displaying this should show the title screen
renderTest1 = render (Game [] [] [] 0)


-- displaying this should show the initial game state. Previously translated pictures wrong relative to the grid
renderTest2 = render initialGamestate


-- computes the AI move when only S is valid. Previously bug when the snake would move out of the board. 
computeAiMoveTest  = TestCase (assertEqual "computeAiMoveTest" (computeAiMove [[Void],[Void],[Snake E red],[Snake E red]] [(red, E, 4), (red, E, 3)] []) S)


-- computes a new food position. Previously infinite loop.
spawnNewTest = TestCase (assertEqual "SpawnNewTest" (spawnNew [] [[Void],[Void],[Snake E red],[Snake E red]] [[(red, E, 4), (red, E, 3)]] 10) ([1], 1))


-- self collision. Previously did not detect self collision, and then did not remove all snakes from all spaces
selfCollisionTest = TestCase (assertEqual "selfCollisionTest" (eliminateCollidingSnakes board positions) ([[Void], [Void], [Void], [Void]], []))
   where board = [[Snake N red], [Snake W red], [Snake N red, Snake E red], [Snake S red]]
         positions = [[(red, N, 3), (red, N, 1), (red, W, 2), (red, S, 4), (red, E, 3)]]


-- trying to change direction of snake on a space outside board, previously crashed the game
-- < 1
changeDirNegativeTest = TestCase (assertEqual "changeDirNegativeTest" (changeDirSpace (red, S, (-1)) board 1) (board))
  where board = [[Snake S red], [Void], [Void], [Void]]

-- > width*height
changeDirOutsideTest = TestCase (assertEqual "changeDirOutsideTest" (changeDirSpace (red, N, (5)) board 1) (board))
  where board = [[Void], [Void], [Snake N red], [Void]]



