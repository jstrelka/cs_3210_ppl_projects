-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author: Thyago Mota
-- Date: 10/10/19

import System.Environment
import System.IO
import Data.List

type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

-- converts given parameter to integer
-- input: a string
-- output: the string converted to integer
-- example: toInt "123" returns 123
toInt :: [Char] -> Int
toInt s = read s :: Int

-- converts given parameter to a sequence of integers (one digit at a time)
-- input: a string
-- output: the string converted into a sequence of integers
-- example: toIntList "123" returns [1, 2, 3]
toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]

-- ***** BASIC GETTER FUNCTIONS *****

-- TODO: convert given string to a sudoku board
-- input: a string (the board as read from a sudoku input file)
-- output: a sudoku board
-- example: getBoard "530070000\n600195000\n098000060\n800060003\n400803001\n700020006\n060000280\n000419005\n000080079\n" returns
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- hint: use lines to convert the string into a list of strings, and then apply toIntList on each of the strings of the list to return the board
getBoard :: [Char] -> Board
getBoard contents = [toIntList row | row <- (lines contents) ]

-- TODO: given a board, return its number of rows
-- input: a board
-- output: number of rows
-- example: getNRows
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns 9
-- hint: use length
getNRows :: Board -> Int
getNRows board = length board

-- TODO: given a board, return its number of columns or 0 if rows do not have the same number of columns
-- input: a board
-- output: number of columns
-- example 1: getNCols
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns 9
-- example 2: getNCols
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,6,0],
--   [8,0,0,0,6,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns 0
-- hint: use length to create a list with all the sizes of each row from the board; then decide whether all of the rows have the same size, returning that size if yes, or 0 otherwise
getNCols :: Board -> Int
getNCols board
  | length (nub colsPerRow) == 1 = head colsPerRow
  | otherwise = 0
  where
    colsPerRow = [ length row | row <- board ]

-- TODO: given a board and box coordinates, return the correspondent box as a sequence
-- input: a board and two integer (box coordinates)
-- output: a sequence
-- example: getBox
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 1 1 returns [0,8,0,6,0,2,0,3,0]
-- hint: use list comprehension to filter the rows of the target box; then transpose what you got and apply the same reasoning to filter the columns; use concat to return the sequence
getBox :: Board -> Int -> Int -> Sequence
getBox board i j = concat cols
  where
    cols = [ (transpose rows)!!k | k <- [j*3 .. (j*3+2)] ]
    rows = [ board!!k | k <- [(i*3) .. (i*3+2)] ]

-- TODO: given a board, return the first location that is empty (i.e., it has zero), if one exists; OK to assume that you will only call this function when you know that there is an empty spot
-- input: a board
-- output: a tuple with the coordinates (i, j) of the empty spot found
-- example: getEmptySpot
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns (0,2)
-- hint: use list comprehension to generate all the coordinates of the board that are empty; use head to return the first coordinate of your list
getEmptySpot :: Board -> (Int, Int)
getEmptySpot board = head [ (i, j) | i <- [0..8], j <- [0..8], (board!!i)!!j == 0 ]

-- ***** PREDICATE FUNCTIONS *****

-- TODO: given a board, return True/False depending whether the given board constitutes a valid grid (i.e., #rows = #cols = 9) or not
-- input: a board
-- output: True/False
-- example 1:
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns True
-- example 2:
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns False
-- example 3:
-- [ [5,3,0,7,0,0,0,0],
--   [6,0,1,9,5,0,0,0],
--   [8,0,0,6,0,0,0,3],
--   [4,0,8,0,3,0,0,1],
--   [7,0,0,2,0,0,0,6],
--   [0,0,0,0,0,2,8,0],
--   [0,0,4,1,9,0,0,5],
--   [0,0,0,8,0,0,7,9] ] returns False
-- hint: use getNRows and getNCols
isGridValid :: Board -> Bool
isGridValid board = and [((getNRows board) == 9), ((getNCols board) == 9)]

-- TODO: return True/False depending whether the given sequence is valid or not, according to sudoku rules
-- input: a sequence of digits from 0-9
-- output: True/False
-- example 1: isSequenceValid [5,3,0,0,7,0,0,0,0] returns True
-- example 2: isSequenceValid [5,3,0,5,7,0,0,0,0] returns False
-- hint: build a list with the digits from the given sequence that are different than zero; then determine whether there are digits that repeats in the created list
isSequenceValid :: Sequence -> Bool
isSequenceValid sqn
  | length (nub nonZeros) == length nonZeros = True
  | otherwise = False
  where
    nonZeros = [ d | d <- sqn, d /= 0 ]

-- TODO: return True/False depending whether ALL of the row sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use list comprehension and isSequenceValid
areRowsValid :: Board -> Bool
areRowsValid board = and [ isSequenceValid row | row <- board ]

-- TODO: return True/False depending whether ALL of the col sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use areRowsValid of the transposed board
areColsValid :: Board -> Bool
areColsValid board = and [ isSequenceValid col | col <- transpose board ]

-- TODO: return True/False depending whether ALL of the box sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use list comprehension, isSequenceValid, and getBox
areBoxesValid :: Board -> Bool
areBoxesValid board = and [ isSequenceValid (getBox board i j) | i <- [0..2], j <- [0..2] ]

-- TODO: return True/False depending whether the given board is valid sudoku configuration or not
-- input: a board
-- output: True/False
-- hint: use isGridValid, areRowsValid, areColsValid, and areBoxesValid
isValid :: Board -> Bool
isValid board = and [ isGridValid board, areRowsValid board, areColsValid board, areBoxesValid board ]

-- TODO: return True/False depending whether the given board is completed or not; a board is considered completed if there isn't a single empty cell
-- input: a board
-- output: True/False
-- hint: use list comprehension and the elem function
isCompleted :: Board -> Bool
isCompleted board = and [ not (elem 0 row) | row <- board ]

-- TODO: return True/False depending whether the given board is solved or not; a board is solved if it is completed and still valid
-- input: a board
-- output: True/False
isSolved :: Board -> Bool
isSolved board = and [ isCompleted board, isValid board ]

-- ***** SOLVER FUNCTIONS *****

-- TODO: given a sequence, an index, and a value, writes the value at the index location, returning a new sequence, but only if the original value at the specified location is empty; otherwise, return the original sequence unchanged
-- input: a sequence, an index, and a value
-- output: a new sequence
-- example 1: setRowAt [1, 2, 3, 0, 4, 5] 3 9 returns [1,2,3,9,4,5]
-- example 2: setRowAt [1, 2, 3, 8, 4, 5] 3 9 returns [1,2,3,8,4,5]
-- hint: use concatenation, take, and drop
setRowAt :: Sequence -> Int -> Int -> Sequence
setRowAt sqn i v
  | sqn!!i /= 0 = sqn
  | otherwise = take i sqn ++ [v] ++ drop (i + 1) sqn

-- TODO: given a board, two indexes i and j (representing coordinates), and a value, writes the value at the (i, j) coordinate, returning the new board, but only if the original value at the specified location is empty; otherwise, return the original board unchanged
-- input: a board, two indexes (i, j), and a value
-- output: a new board
-- example 1: setBoardAt
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 4 returns
-- [ [5,3,4,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- hint: use concatenation and setRowAt
setBoardAt :: Board -> Int -> Int -> Int -> Board
setBoardAt board i j v = [ board!!k | k <- [0..(i-1)] ] ++ [setRowAt (board!!i) j v] ++ [ board!!k | k <- [(i+1)..((length board) - 1)] ]

-- TODO: given a board and a two indexes i and j (representing coordinates), generate ALL possible boards, replacing the cell at (i, j) with ALL possible digits from 1 to 9; OK to assume that the cell at (i, j) is empty
-- input: a board and two indexes (i, j)
-- output: a list of boards from the original board
-- example: buildChoices
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 returns
-- [
-- [ [5,3,1,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- [ [5,3,2,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- ...
-- [ [5,3,9,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- ]
-- hint: use list comprehension and the function setBoardAt
buildChoices :: Board -> Int -> Int -> [Board]
buildChoices board i j = [ setBoardAt board i j d | d <- [1..9] ]

-- given a board, finds all possible solutions (note that dead ends or invalid intermediate solutions are listed as empty boards)
-- input: a board
-- output: a list of boards from the original board
solve :: Board -> [Board]
solve board
  | isSolved board = [board]
  | isCompleted board = [[[]]]
  | not (isValid board) = [[[]]]
  | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
    where
      emptySpot = getEmptySpot board
      i = fst emptySpot
      j = snd emptySpot

-- program starts here
main = do

  -- TODO: validate the command-line and get the file name containing the board
  args <- getArgs
  if (length args) == 0
    then error "Missing sudoku board files!"
    else return ()
  let fileName = head args

  -- TODO: read the contents of the board file into a string
  contents <- readFile fileName

  -- TODO: create a board from the string board (hint: use getBoard)
  let board = getBoard contents

  -- TODO: use solve to find the solutions, disconsidering the ones that are [[]]
  let solutions = [ solution | solution <- solve board, solution /= [[]] ]

  -- TODO: print the solutions found
  print solutions
