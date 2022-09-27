data SnakeGame = Game Board [[Pos]] [Float] Float deriving (Show)


{- Pos
   Represents a piece of a snake (c, dir, pos), where
   c gives the color of the snake,
   dir the direction the piece is headed and
   pos its position on the board.
-}
type Pos = (Int, Direction ,Float)     


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
data Object = Snake Direction Int | Food | Void deriving (Eq, Show)




{- Direction
   Describes the direction of a Snake Object,
   where N means it is headed North, S south, W west, E east.
-}
data Direction = N | S | W | E deriving (Show, Eq)









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
RETURNS: a list containing every position on the board containing Void
EXAMPLE: emptySpacesAUX [[Void][Void][Food]] = [1.0,2.0]
emptySpacesAux [[Food],[Food],[Void]] = [3.0]
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

