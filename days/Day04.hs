module Main (main) where

import Paths_advent_y2022 (getDataFileName)
import Data.Bifunctor (bimap)

main :: IO ()
main = do 
    putStr "Part 1: "
    d <- count . fmap fullyContains <$> elves 
    print d

    putStr "Part 2: "
    d' <- count . fmap partiallyContains <$> elves 
    print d'

count :: [Bool] -> Int
count = length . filter (== True)

fullyContains :: Ord a => ((a, a), (a, a)) -> Bool
fullyContains ((a, b), (c, d)) = a >= c && b <= d || c >= a && d <= b

partiallyContains :: Ord a => ((a, a), (a, a)) -> Bool
partiallyContains ((a, b), (c, d))
    | a >= c && a <= d = True
    | b >= c && b <= d = True
    | c >= a && c <= b = True
    | d >= a && d <= b = True
    | otherwise        = False

elves :: IO [((Int, Int), (Int, Int))]
elves = fmap (bimap readSplit readSplit . span2 ',') . lines
     <$> (getDataFileName "Day04.txt" >>= readFile)
    where readSplit = bimap read read . span2 '-'
          span2 d fs = let (a, b) = break (== d) fs 
                        in (a, if null b then [] else tail b)