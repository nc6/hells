module Hells.Setter(
    set
  , answer
  , Response(..)
  ) where
  
  import Data.List (intersect, nub, zip)
  import System.Random

  import Hells.Common

  -- | Game place
  data Game = Game Place deriving Show

  -- | Response
  data Response = TryAgain Int Int -- ^ TryAgain bulls cows
                | Victory

  -- | Set a new game
  set :: IO Game
  set = newStdGen >>= \rand ->
    return . Game . Place $ take 4 . nub $ randomRs (0,9) rand

  -- | Answer a guess.
  answer :: Game -> Place -> Response
  answer (Game (Place a)) (Place b) = case (bulls, cows) of
      (4, 0) -> Victory
      (b, c) -> TryAgain bulls cows
    where
      cows = (length $ intersect a b) - bulls
      bulls = length . filter (\(x,y) -> x == y) $ zip a b