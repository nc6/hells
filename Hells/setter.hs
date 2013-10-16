module Hells.Setter(
    set
  , answer
  , Game()
) where
  
  import Data.List (nub)
  import System.Random

  import Hells.Common

  -- | Game solution
  data Game = Game Guess deriving Show

  -- | Set a new game
  set :: IO Game
  set = newStdGen >>= \rand ->
    return . Game . Guess $ take 4 . nub $ randomRs (0,9) rand

  -- | Answer a guess.
  answer :: Game -> Guess -> Response
  answer (Game (Guess s)) (Guess g) = score s g