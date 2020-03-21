{-# LANGUAGE ParallelListComp #-}

-- CS 3210 - Principles of Programming Languages - Spring 2020
-- Programming Assignment 02 - The N-queens Problem
-- Author(s):

import Data.List

type Seq   = [Char]
type Board = [Seq]

-- TODO 01/17
setup :: Int -> Board
setup n
    | n < 4 = [ln | ln <- replicate 4 (replicate 4 '-')]
    | otherwise = [ln | ln <- replicate n (replicate n '-')]

-- TODO 02/17
rows :: Board -> Int
rows b = length b

-- TODO 03/17
cols :: Board -> Int
cols b 
    | length(nub (map length b)) == 1 = length b
    | otherwise = 0

---cols b
---    | length(nub b) == 1 = length b
---    | otherwise = 0

-- TODO 04/17
size :: Board -> Int
size b
    | cols b == rows b = rows b
    | otherwise = 0

-- TODO 05/17
queensSeq :: Seq -> Int
queensSeq s = length( filter (=='Q') s )

-- TODO 06/17
queensBoard :: Board -> Int
queensBoard b = length( filter (=='Q') (unwords b) )

-- TODO 07/17
seqValid :: Seq -> Bool
seqValid s
    | queensSeq s < 2 = True
    | queensSeq s >= 2 = False

-- TODO 08/17
rowsValid :: Board -> Bool
rowsValid b
    | size b == 0 = True
    | seqValid (b!!((length b) - 1)) == False = False
    | otherwise = rowsValid(init b)

-- TODO 09/17
colsValid :: Board -> Bool
colsValid b
    | size b == 1 = True
    | length [ i | i <- [0 .. ((length (b!!0)) - 1)], j <- [1 ..  ((size b) - 1)], (b!!0)!!i == 'Q', (b!!0)!!i == (b!!j)!!i ] > 0 = False
    | otherwise = colsValid (tail b)

-- TODO 10/17
diagonals :: Board -> Int
diagonals b = (2 * (size b) - 1)


main = print(diagonals ["-Q---","Q----","--QQ-","---Q-","-----"] )