module Main (main) where
    
import Paths_advent_y2022 (getDataFileName)

data HandShape = Rock 
               | Paper 
               | Scissors 
               
handScore :: HandShape -> Int
handScore Rock = 1 
handScore Paper = 2
handScore Scissors = 3

data Result = Win 
            | Draw 
            | Lose

resultScore :: Result -> Int 
resultScore Win = 6
resultScore Draw = 3
resultScore Lose = 0

handFor :: Game -> HandShape
handFor (Rock, Win)      = Paper
handFor (Rock, Lose)     = Scissors
handFor (Paper, Win)     = Scissors
handFor (Paper, Lose)    = Rock
handFor (Scissors, Win)  = Rock
handFor (Scissors, Lose) = Paper
handFor (q, Draw)        = q

type Game = (HandShape, Result)

gameScore :: Game -> Int
gameScore g@(_, res) = resultScore res + handScore (handFor g)

main :: IO ()
main = do 
    putStrLn "Day 02 Part 2"
    
    getDataFileName "Day02.txt" 
    >>= readFile
    >>= print 
      . sum
      . fmap (gameScore . parseGame) 
      . lines

parseGame :: String -> Game
parseGame [opponent, ' ', me] = (readHand opponent, readResult me)
parseGame _ = error "Bad parse statement"

readHand :: Char -> HandShape
readHand 'A' = Rock
readHand 'B' = Paper
readHand 'C' = Scissors
readHand _ = error "Bad input"

readResult :: Char -> Result
readResult 'X' = Lose
readResult 'Y' = Draw 
readResult 'Z' = Win
readResult _ = error "Bad input"
