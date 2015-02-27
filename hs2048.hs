module Hs2048 where

import Data.List
import System.Random (randomRIO)
import Control.Monad (when, unless)
import Control.Applicative

data Action = UP | DOWN | LEFT | RIGHT | NOP deriving (Show, Eq)

type Score = Int

type Line = [Int]

type Matrix = [Line]

type Game = (Score, Matrix)

parseAct :: String -> Action
parseAct ('w':_) = UP
parseAct ('s':_) = DOWN
parseAct ('a':_) = LEFT
parseAct ('d':_) = RIGHT
parseAct  _  = NOP

emptyGame = (0, replicate 4 $ replicate 4 0) :: Game

rot :: Matrix -> Matrix
rot (a:b:c:d:[]) = zipWith4 (\ a b c d -> [a, b, c, d]) a b c d

isTerminaled :: Matrix -> Bool
isTerminaled = (||) <$> isTerminaledX <*> isTerminaledY
  where
    isTerminaledLine xs = (not $ elem 0 xs) && ((==4) $ length $ nub xs)
    isTerminaledX = all isTerminaledLine
    isTerminaledY = isTerminaledX . rot

listHole :: Matrix -> [Int]
listHole = findIndices (==0) . concat

setCell :: Matrix -> Int -> Int -> Matrix
setCell mtx index value =
  reform $ take index cells ++ [value] ++ drop (index + 1) cells
  where
    cells = concat mtx
    reform [] = [] 
    reform xs = (take 4 xs) : (reform $ drop 4 xs)

randAdd :: Int -> Game -> IO Game
randAdd v game@(s, mtx) = do
  let holes = listHole mtx
  if null holes
     then return game
     else do index <- randomRIO (0, length holes - 1)
             return (s, setCell mtx (holes !! index) v)

moveLine :: Action -> Line -> (Line, Int)
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

foldMatrix :: [(Line, Score)] -> Game
foldMatrix = (,) <$> (sum . map snd) <*> (map fst)

foldMatrixRot :: [(Line, Score)]  -> Game
foldMatrixRot = ((,) <$> fst <*> rot . snd) `fmap` foldMatrix 

move :: Game -> Action -> Game
move game act = game `addScore` move' act game
  where
    move' NOP   = id
    move' UP    = foldMatrixRot . map (moveLine LEFT)  . rot . snd
    move' DOWN  = foldMatrixRot . map (moveLine RIGHT) . rot . snd
    move' act   = foldMatrix . map (moveLine act) . snd
    addScore (s, _) (ds, mtx) = (s + ds, mtx)

dump :: Game -> String
dump (score, mtx) =
  unlines $ ("Score :" ++ show score) : mtxlines
  where mtxlines = map (unwords . map show) mtx

echo :: Game -> IO Game
echo game = (putStr . dump $ game) >> return game

getAct :: IO Action
getAct = fmap parseAct getLine

mainloop :: Game -> IO ()
mainloop game@(s, mtx) = do
  unless (isTerminaled mtx) $ do
    getAct >>= randAdd 2 . move game
           >>= echo
           >>= mainloop

main :: IO ()
main =
  randAdd 2 emptyGame
    >>= randAdd 2
    >>= echo
    >>= mainloop 

