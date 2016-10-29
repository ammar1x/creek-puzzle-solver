
module Board
(  BoardField(..)
,  BoardSize
,  Board(..)
,  newBoard
,  fillBoardFieldAt
,  fillBoardFieldsAt
,  getFilledFields
,  getEmptyFields
,  isValidFieldIndex
,  getNeigboursFields
,  getEmptyNeigbours
,  hasEmptyNeigbours
,  isEmpty ) where
-- remove the line below
import Data.List
{-

This file defines data types and functions related to a Board.

-}

type BoardSize = (Int, Int)
-- A BoardField is either empty or filled (Starred).
data BoardField = Empty | Filled deriving (Eq, Read)
-- Data type for a Board. A Board is 2-dimensional array of board fields.
data Board = Board BoardSize [[BoardField]] deriving Eq

-- Make a new board with empty fields.
newBoard :: BoardSize -> Board
newBoard (a, b) = Board (a, b) [replicate b Empty | _ <- [1..a]]

-- Get board field at given coordinates.
getBoardFieldAt (Board _ fields) (i, j) = (fields !! i) !! j

-- Set board field at given coordinates.
setBoardFieldAt (Board (x, y) fields) (i, j) v = Board (x, y)
 [ [if i == rowIndex && j == colIndex then v else field | (field, colIndex) <- zip row [0..(y-1)]] | (row, rowIndex) <- zip fields [0..(x-1)]]

-- Set board fields with given list of coordinates.
setBoardFieldsAt b@(Board (x,y) fields) [] = b
setBoardFieldsAt b@(Board (x,y) field) ((ij, v):ls) = setBoardFieldsAt (setBoardFieldAt b ij v ) ls

-- Fill field at given coordinates.
fillBoardFieldAt b ij = setBoardFieldAt b ij Filled

-- Empty field at given coordinates.
emptyBoardFieldAt b ij = setBoardFieldAt b ij Empty

-- Fill board fields.
fillBoardFieldsAt b [] = b
fillBoardFieldsAt b (ij:xs) = fillBoardFieldAt (fillBoardFieldsAt b xs) ij

-- Examine whether a field at given coordinates is empty.
isEmpty (Board (x,y) fields) (i,j) = (fields !! i) !! j == Empty

-- Return a list of filled fields for the given board.
getFilledFields :: Board -> [(Int, Int)] -> [(Int, Int)]
getFilledFields b xs = [x | x<-xs, not $ isEmpty b x]

-- Return a list of empty fields for the given board.
getEmptyFields :: Board -> [(Int, Int)] -> [(Int, Int)]
getEmptyFields b xs = filter (isEmpty b) xs

-- Check whether a given pair of indices is valid for given board.
isValidFieldIndex :: Board -> (Int, Int) -> Bool
isValidFieldIndex b@(Board (x,y) _) (i,j)
                                | i < 0 || i >= x   = False
                                | j < 0 || j >= y   = False
                                | otherwise         = True

-- Return the fields that are neigbouring to the given field.
getNeigboursFields :: Board -> (Int, Int) -> [(Int, Int)]
getNeigboursFields b (i,j) = concat [ [(fi,j)| fi <- [i-1,i+1], isValidFieldIndex b (fi, j)],
                                              [(i, fj) | fj <- [j-1, j+1], isValidFieldIndex b (i, fj)] ]

-- Return a list of empty neigbours for the given field.
getEmptyNeigbours :: Board -> (Int, Int) -> [(Int, Int)]
getEmptyNeigbours b ij = [x | x <- getNeigboursFields b ij, isEmpty b x]

-- Check whether a field has empty neigbours.
hasEmptyNeigbours :: Board -> (Int, Int) -> Bool
hasEmptyNeigbours b@(Board (x,y) _) (i, j)
                        | i == 0 && j == 0          = n > 0
                        | i == x-1 && j == y -1     = n > 0
                        | i == 0 && j == y -1       = n > 0
                        | j == 0 && i == x - 1      = n > 0
                        | otherwise                 = n > 1
                        where emptyNeigbours =  getEmptyNeigbours b (i,j)
                              n = length emptyNeigbours


-- toString related functions:

instance Show BoardField where
    show Empty = " "
    show Filled = "*"


instance Show Board where
    show = (flip toStringBoard) (-1) -- change -1 to 0

-- auxiliary functions for toString
toStringRow b@(Board (x, y) fs) (row,col)
                        | col == y      = "|"
                        | otherwise     = field ++ toStringRow b (row, (col+1))
                        where field = "|" ++ show ((fs !! row) !! col)


toStringBoard b@(Board (x, y) fs) row
        | row == -1     = ((++) "   " $intercalate "|" $ map show [0..(x-1)]) ++ "\n" ++ toStringBoard b 0
                                -- last row does not contain '\n'
                                | row == x-1    = show row ++ " "++toStringRow b (row, 0)
                                --
                                | otherwise     = show row ++ " "++toStringRow b (row, 0) ++ "\n" ++ toStringBoard b (row+1)

-- for testing
main = do
       let c = newBoard(5, 5)
       let g = [(x,y) | x <- [0..1], y <- [0..1]]
       let d = fillBoardFieldsAt c g
       print d



