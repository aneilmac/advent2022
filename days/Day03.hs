module Main (main) where

import Data.List (intersect)
import Data.Char (ord, isAsciiLower, isAsciiUpper)
import Paths_advent_y2022 (getDataFileName)

main :: IO ()
main = do
    putStr "Part 1: "
    elves >>= print 
            . sum 
            . fmap (sameItem . compartments)

    putStr "Part 2: "
    elves >>= print 
            . sum 
            . fmap (head . commonOf) 
            . groupBy3

elves :: IO [[Int]]
elves = fmap (fmap priority) 
      . lines 
     <$> (getDataFileName "Day03.txt" >>= readFile)

priority :: Char -> Int
priority a
    | isAsciiLower a = ord a - 97 + 1
    | isAsciiUpper a = ord a - 65 + 27
    | otherwise = error "Bad character"

compartments :: [a] -> ([a], [a])
compartments s = splitAt (length s `div` 2) s

sameItem :: Eq a => ([a], [a]) -> a
sameItem (a, b) = head (a `intersect` b)

groupBy3 :: [a] -> [[a]]
groupBy3 [] = []
groupBy3 as = let (a, as') = splitAt 3 as
               in a : groupBy3 as'
            
commonOf :: Eq a => [[a]] -> [a]
commonOf [] = []
commonOf [a] = a
commonOf (a:as) = a `intersect` commonOf as
