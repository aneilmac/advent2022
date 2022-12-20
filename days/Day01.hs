module Main (main) where

import Paths_advent_y2022 (getDataFileName)
import Data.List (sort)

main :: IO ()
main = do
    putStr "Part 1: "
    elves >>= print . maximum 

    putStr "Part 2: "
    elves >>= print . sum . take 3 . reverse . sort

elves :: IO [Int]
elves = fmap (sum . fmap read)
      . splitEmpty 
      . lines 
      <$> (getDataFileName "Day01.txt" >>= readFile)

splitEmpty :: (Eq a, Monoid a) => [a] -> [[a]]
splitEmpty [] = []
splitEmpty es = let (e, es') = break (== mempty) es
                 in e : if null es' then [] else splitEmpty (tail es')