import System.IO            -- File api
import Board                -- Board module
import Data.List            -- sortBy
import Utils                -- for combination
import Debug.Trace          -- Trace function

{-
    This file has functions and data types for solving creek puzzle game.
-}

-- Represent an intersection. Intersection has coordinates and size.
type Intersection = ((Int, Int), Int)

-- A list of intersections.
type Intersections = [Intersection]

-- Creek game data representation.
data Creek = Creek BoardSize Intersections deriving (Show, Read)

-- Get the board size.
getBoardSize :: Creek -> BoardSize
getBoardSize (Creek sz _ ) = sz

-- Get intersections.
getIntersections :: Creek -> Intersections
getIntersections (Creek sz xs) = xs

-- Get intersections sorted descendingly by size.
sortedIntersectionsBySize :: Creek -> Intersections
sortedIntersectionsBySize (Creek sz xs) = sortBy (\(a1,b1) (a2,b2) -> b2 `compare` b1) xs

-- Examine if the given board has creek.
-- A board has a creek if all empty fields are connected.
hasCreek :: Board -> Bool
hasCreek b@(Board (x,y) _) = and [hasEmptyNeigbours b (i,j) | i <-  [0..(x-1)],
                                                    j <- [0..(y-1)], isEmpty b (i,j)]

-- Return fields for the given intersection.
getFieldsForIntersection :: Board -> Intersection -> [(Int, Int)]
getFieldsForIntersection b@(Board (x, y) fs) ((i, j), _)
                                |  i == 0  && j == 0    = [(0, 0)]
                                |  i == x  && j == 0    = [(x-1, 0)]
                                |  i == 0  && j == y    = [(0, y-1)]
                                |  i == x  && j == y    = [(x-1, y-1)]
                                |  j == 0               = [(fi, j) | fi <- [i-1..i]]
                                |  i == 0               = [(i, fj) | fj <- [j-1..j]]
                                |  i == x               = [(i-1, fj) | fj <- [j-1..j]]
                                |  j == y               = [(fi, j-1)   | fi <- [i-1..i]]
                                |  otherwise            = [(fi, fj) | fi <- [(i-1)..i], fj <- [(j-1)..j]]



-- Check if the number of fields given by the intersection equals the size of the intersection.
isBoardFieldsFilledForInters :: Board -> Intersection -> Bool
isBoardFieldsFilledForInters b x@((i,j), sz) = length (filledFields) == sz
                                where filledFields = [y | y <- getFieldsForIntersection b x, not (isEmpty b y)]

-- Check if all intersections are filled.
isBoardFieldsFilledForAllInters :: Board -> Intersections -> Bool
isBoardFieldsFilledForAllInters b xs = and [isBoardFieldsFilledForInters b x | x <- xs]

-- Check if the proposed solution is valid or not.
isValidSolution :: Board -> Intersections -> Bool
--for debugging
--isValidSolution b xs | trace (show b) False = undefined
-- debugging

isValidSolution b xs = hasCreek b && isBoardFieldsFilledForAllInters b xs




solve :: Board -> Intersections -> Intersections -> Int -> Maybe Board
solve b [] _ _ = Just b
-- for debugging only
-- solve b (x:xs) ys k | trace ("solve " ++ show x ++ " " ++ show ys ++ " " ++ show k) False = undefined
solve b (x:xs) ys k =
                if k == z
                    then
                        Nothing
                    else
                        if (isValidSolution b1 ys)
                            then
                                if b2 == Nothing
                                    then
                                        solve b (x:xs) ys (k+1)
                                    else
                                        b2
                            else
                                solve b (x:xs) ys (k+1)
                    where
                        (_, sz) = x
                        n = length filledFs
                        m = length emptyFs
                        filledFs = getFilledFields b fieldsIntersection
                        emptyFs = getEmptyFields b fieldsIntersection
                        fieldsIntersection = getFieldsForIntersection b x

                        fieldsCombs = comb sz fieldsIntersection
                        z = length fieldsCombs

                        b1 = chooser b fieldsCombs k
                        -- for debugging
                        --chooser b comb k | trace ("combination komb " ++ show (comb !! k)) False = undefined
                        chooser b comb k = fillBoardFieldsAt b (comb !! k)

                        b2 = solve b1 xs (x:ys) 0


-- Solve the game and report results to the output stream.
solveAndReport :: Board -> Intersections -> IO ()
solveAndReport b xs = case solve b xs [] 0 of
            Nothing -> do
                        putStrLn "Cannot find solution"
            Just sol -> do
                        putStrLn "Solution:"
                        print sol

-- Entry to the program.
main = do
        putStrLn "Enter the name of the file containing puzzle"
        name <- getLine
        handle <- openFile name ReadMode
        content <- hGetContents handle
        let c = read content :: Creek
        let board = newBoard (getBoardSize c)
        let intersection = sortedIntersectionsBySize c
        solveAndReport board intersection
        hClose handle




