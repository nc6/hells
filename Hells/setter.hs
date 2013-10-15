module Hells.Setter(
    set
  , answer
  , Game()
) where
  
  import Data.List (intersect, nub, zip)
  import System.Random

  import Hells.Common

  type Solution = Guess
  -- | Game place
  data Game = Game Solution deriving Show

  -- | Set a new game
  set :: IO Game
  set = newStdGen >>= \rand ->
    return . Game . Guess $ take 4 . nub $ randomRs (0,9) rand

  -- | Answer a guess.
  answer :: Game -> Guess -> Response
  answer (Game (Guess a)) (Guess b) = case (bulls, cows) of
      (4, 0) -> Victory
      (b, c) -> TryAgain bulls cows
    where
      cows = (length $ intersect a b) - bulls
      bulls = length . filter (\(x,y) -> x == y) $ zip a b