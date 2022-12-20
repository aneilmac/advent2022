module Main (main) where
    
import Paths_advent_y2022 (getDataFileName)

data HandShape = Rock | Paper | Scissors deriving (Show)

handScore :: HandShape -> Int
handScore Rock = 1 
handScore Paper = 2
handScore Scissors = 3

data Result = Win | Draw | Lose deriving (Show)

resultScore :: Result -> Int 
resultScore Win = 6
resultScore Draw = 3
resultScore Lose = 0

result :: Game -> Result
result (Rock, Paper)        = Win
result (Paper, Scissors)    = Win
result (Scissors, Rock)     = Win
result (Rock, Rock)         = Draw
result (Paper, Paper)       = Draw
result (Scissors, Scissors) = Draw
result (Rock, Scissors)     = Lose
result (Paper, Rock)        = Lose
result (Scissors, Paper)    = Lose

type Game = (HandShape, HandShape)

gameScore :: Game -> Int
gameScore g@(_, me) = resultScore (result g) + handScore me

main :: IO ()
main = do 
    putStrLn "Day 02 Part 1"
    
    getDataFileName "Day02.txt" 
    >>= readFile
    >>= print 
      . sum 
      . fmap (gameScore . parseGame) 
      . lines

parseGame :: String -> Game
parseGame [opponent, ' ', me] = (readHand opponent, readHand me)
parseGame _ = error "Bad parse statement"

readHand :: Char -> HandShape
readHand 'A' = Rock
readHand 'X' = Rock

readHand 'B' = Paper
readHand 'Y' = Paper

readHand 'C' = Scissors
readHand 'Z' = Scissors
readHand _ = error "Bad input"
