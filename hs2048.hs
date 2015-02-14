module Main where

import Data.List
import System.Random (randomRIO)
import Control.Monad (when, unless)

data Action = UP | DOWN | LEFT | RIGHT | NOP deriving (Show, Eq)

data Game = Game { score :: Int, matrix :: [[Int]] } deriving (Show)

parseAct ('w':_) = UP
parseAct ('s':_) = DOWN
parseAct ('a':_) = LEFT
parseAct ('d':_) = RIGHT
parseAct  _  = NOP

testmtx = [[2,0,2,0], [2,0,2,0], [0,2,0,2], [2,0,2,0]]

rot (a:b:c:d:[]) = zipWith4 (\ a b c d -> [a, b, c, d]) a b c d

isTerminaledLine xs = (not $ elem 0 xs) && ((==4) $ length $ nub xs)
isTerminaledX = all isTerminaledLine
isTerminaledY = isTerminaledX . rot
isTerminaled mtx = isTerminaledX mtx || isTerminaledY mtx

listHole = findIndices (==0) . concat

setCell mtx index value =
    reform $ take index cells ++ [value] ++ drop (index + 1) cells
    where
        cells = concat mtx
        reform [] = [] 
        reform xs = (take 4 xs) : (reform $ drop 4 xs)

randAdd v game@(Game s mtx) = do
    let holes = listHole mtx
    index <- randomRIO (0, length holes - 1)
    return $ if null holes
                then game
                else (Game s (setCell mtx (holes !! index) v))

moveLine UP    xs = moveLine LEFT  xs
moveLine DOWN  xs = moveLine RIGHT xs
moveLine RIGHT xs =
    let (l, s) = moveLine LEFT . reverse $ xs
    in  (reverse l, s)
moveLine LEFT  xs@(a:b:c:d:[]) = 
    let xs' =  filter (/=0) xs
        xs'' = xs' ++ replicate (4 - length xs') 0
    in  moveLeft xs''
moveLeft  xs@(a:b:c:d:[]) 
    | a == b && c == d = ([a + b, c + d, 0, 0], sum xs)
    | a == b = ([a + b, c, d, 0], a + b)
    | b == c = ([a, b + c, d, 0], b + c)
    | c == d = ([a, b, c + d, 0], c + d)
    | otherwise = (xs, 0)

foldmtx xs = (map fst xs, sum $ map snd xs)
foldmtxrot xs = let (n, s) = foldmtx xs in (rot n, s)

move NOP   = \x -> (x, 0)
move UP    = foldmtxrot . map (moveLine LEFT)  . rot
move DOWN  = foldmtxrot . map (moveLine RIGHT) . rot
move act   = foldmtx . map (moveLine act)

dump game =
     unlines $ scoreline:mtxlines
     where
        scoreline = concat ["Score: ", show $ score game]
        mtxlines  = map (unwords . map show) $ matrix game

echo = fmap putStrLn dump

evo act (Game s mtx) =
    let (nmtx, ds) = move act mtx 
    in  Game (s + ds) nmtx

getAct = fmap parseAct getLine

game = Game { score = 0, matrix = testmtx }

mainloop game@(Game s mtx) = do
    unless (isTerminaled mtx) $ do
        act <- getAct
        ng  <- randAdd 2 $ evo act game
        echo ng >> mainloop ng

main = echo game >> mainloop game
