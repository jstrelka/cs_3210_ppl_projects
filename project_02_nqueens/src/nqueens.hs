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
    | n < 4 = [line | line <- replicate 4 (replicate 4 '-')]
    | otherwise = [line | line <- replicate n (replicate n '-')]

-- TODO 02/17
rows :: Board -> Int
rows b = length b

-- TODO 03/17
cols :: Board -> Int
cols b 
    | length(nub (map length b)) == 1 = length b
    | otherwise = 0

-- TODO 04/17
size :: Board -> Int
size b
    | cols b == rows b = rows b
    | otherwise = 0

-- TODO 05/17
queensSeq :: Seq -> Int
queensSeq s = length(filter (=='Q') s )

-- TODO 06/17
queensBoard :: Board -> Int
queensBoard b = length( filter (=='Q') (unwords b))

-- TODO 07/17
seqValid :: Seq -> Bool
seqValid s
    | queensSeq s < 2 = True
    | queensSeq s >= 2 = False

-- TODO 08/17
rowsValid :: Board -> Bool
rowsValid b
    | length b == 0 = True
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

mainDiagIndices :: Board -> Int -> [ (Int, Int) ]
mainDiagIndices b p
  | p < n = [ (n - 1 - qr, q) | q <- [0..p] | qr <- [p,p-1..0] ]
  | otherwise = [ (q, (n - 1 - qr)) | q <- [0..2 * (n - 1) - p] | qr <- [2 * (n - 1) - p,2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODO 11/17
allMainDiagIndices :: Board -> [[ (Int, Int) ]]
allMainDiagIndices b = [ mainDiagIndices b n| n <- [0 .. ((diagonals b) -1)] ]

-- TODO 12/17
mainDiag :: Board -> [Seq]
mainDiag b = [ lst | x <- [0 .. ((diagonals b) - 1)], let lst = [ a | let y = mainDiagIndices b x, z <- [0 .. ((length y) - 1)], let a = (b!!(fst (y!!z)))!!(snd (y!!z))] ]

secDiagIndices :: Board -> Int -> [ (Int, Int) ]
secDiagIndices b p
  | p < n = [ (p - q, q) | q <- [0..p] ]
  | otherwise = [ (p - (n - 1 - q), n - 1 - q) | q <- [2 * (n - 1) - p, 2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODO 13/17
allSecDiagIndices :: Board -> [[ (Int, Int) ]]
allSecDiagIndices b = [ secDiagIndices b n| n <- [0 .. ((diagonals b) -1)] ]

-- TODO 14/17
secDiag :: Board -> [Seq]
secDiag b = [ lst | x <- [0 .. ((diagonals b) - 1)], let lst = [ a | let y = secDiagIndices b x, z <- [0 .. ((length y) - 1)], let a = (b!!(fst (y!!z)))!!(snd (y!!z))] ]

-- TODO 15/17
diagsValid :: Board -> Bool
diagsValid b
    | length ([ md | let i = mainDiag b, j <- [0 .. ((length i) - 1)], let l = i!!j, length(filter (=='Q') l) > 1, let md = l]) > 0 = False
    | length ([ md | let i = secDiag b, j <- [0 .. ((length i) - 1)], let l = i!!j, length(filter (=='Q') l) > 1, let md = l]) > 0 = False
    | otherwise = True

-- TODO 16/17
valid :: Board -> Bool
valid b = False

-- TODO 17/17 (¡Phew!)
solved :: Board -> Bool
solved b = False

setQueenAt :: Board -> Int -> [Board]
setQueenAt b i = do
  let z = replicate ((size b) - 1) '-'
  let p = nub (permutations ("Q" ++ z))
  [ [ (b!!k) | k <- [0..(i-1)] ] ++ [r] ++ [ (b!!k) | k <- [(i+1)..((rows b) - 1)] ] | r <- p ]

nextRow :: Board -> Int
nextRow b = head [ i | i <- [0 .. (size b) - 1], queensSeq (b!!i) == 0 ]

solve :: Board -> [Board]
solve b
  | solved b = [b]
  | otherwise = concat [ solve newB | newB <- setQueenAt b i, valid newB ]
    where i = nextRow b

main = do
  let b = setup 6
  let solution = [ solution | solution <- solve b ]
  print (solution)
