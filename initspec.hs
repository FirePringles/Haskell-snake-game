{-initialGamestate
creates an initial game state of type SnakeGame.
RETURNS: a SnakeGame depending on the size of the board and the number of choosen snakes.
PRE: TRUE
EXAMPLES: initialGamestate = Game [[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Snake E (RGBA 1.0 0.0 0.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake E (RGBA 0.0 0.0 1.0 1.0)],[Snake N (RGBA 0.0 0.0 1.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food,Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food,Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Food,Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake S (RGBA 0.0 1.0 0.0 1.0)],[Snake W (RGBA 0.0 1.0 0.0 1.0)],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Void],[Snake W (RGBA 1.0 0.5 0.0 1.0)],[Snake W (RGBA 1.0 0.5 0.0 1.0)]] [[(RGBA 1.0 0.0 0.0 1.0,E,2.0),(RGBA 1.0 0.0 0.0 1.0,E,1.0)],[(RGBA 0.0 0.0 1.0 1.0,N,16.0),(RGBA 0.0 0.0 1.0 1.0,E,15.0)],[(RGBA 0.0 1.0 0.0 1.0,S,129.0),(RGBA 0.0 1.0 0.0 1.0,W,130.0)],[(RGBA 1.0 0.5 0.0 1.0,W,143.0),(RGBA 1.0 0.5 0.0 1.0,W,144.0)]] [54.0,90.0,72.0] 0.0
-}
initialGamestate = Game boardWithFood pos [food1, food2, food3]  0
  where
    (board, pos) = initialAux (genFromSize width height) [] 4 0
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

-}

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
