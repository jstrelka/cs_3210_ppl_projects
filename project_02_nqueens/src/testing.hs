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
--setup n = [ln | ln <- replicate n (replicate n '-')]

-- TODO 02/17
rows :: Board -> Int
rows b = length b

-- TODO 03/17
cols :: Board -> Int
cols b
    | length(nub b) == 1 = length b
    | otherwise = 0

--    | length b == 1 = rows b
--    | b!!0 == b!!((length b) - 1) = cols(init b)
--    | otherwise = 0
    


main = print(cols ["------","------","------","------","------","------"])