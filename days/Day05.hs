module Main (main) where

import Paths_advent_y2022 (getDataFileName)

import Data.List (transpose)

import Data.Vector (fromList, Vector, (!), modify)
import qualified Data.Vector.Mutable as VM

import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Combinator (many1, endBy)
import Text.Parsec.Char (string, space, digit, endOfLine)

newtype Block = Block Char deriving (Show)
data Move = Move Int Int Int deriving (Show)

main :: IO ()
main = do 
    bs <- blocks
    (Right ms) <- getDataFileName "Day05Moves.txt" >>= parseFromFile moveParser
    
    putStr "Part 1: "
    print $ heads $ foldl (applyMoveToBlocks True) bs ms

    putStr "Part 2: "
    print $ heads $ foldl (applyMoveToBlocks False) bs ms

heads :: Vector [Block] -> String
heads = foldr (\b s -> let (Block c) = head b in c : s) []

blocks :: IO (Vector [Block])
blocks =  fromList
       . fmap (parseBlocks . init) 
       . transpose 
       . fmap groupBy4 
       . lines
      <$> (getDataFileName "Day05Crates.txt" >>= readFile)

groupBy4 :: [a] -> [[a]]
groupBy4 [] = []
groupBy4 as = let (a, as') = splitAt 4 as
               in a : groupBy4 as'

parseBlocks :: [String] -> [Block]
parseBlocks [] = []
parseBlocks ("    ":xs) = parseBlocks xs
parseBlocks (('[':x:']':_):xs) = Block x : parseBlocks xs
parseBlocks _ = error "Bad string"

moveParser :: Parser [Move]
moveParser = moveLineParser `endBy` endOfLine

moveLineParser :: Parser Move
moveLineParser = do
    _ <- string "move" 
    _ <- space
    a <- read <$> many1 digit 
    _ <- space 
    _ <- string "from"
    _ <- space
    b <- read <$> many1 digit
    _ <- space 
    _ <- string "to"
    _ <- space
    c <- read <$> many1 digit
    return $ Move a (b - 1) (c - 1)

applyMoveToBlocks :: Bool -> Vector [Block] -> Move -> Vector [Block]
applyMoveToBlocks r bs (Move amount from to) = 
    let a   = take amount $ bs ! from -- crates to take from stack "from"
        a'  = if r then reverse a else a  -- Crane 9000 or 9001
        bs' = modify (\v -> VM.modify v (drop amount) from) bs -- crates removed
      in modify (\v -> VM.modify v (a' ++) to) bs' -- crates appended
