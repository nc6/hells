module Hells.Common where

  import Data.List (intersect, zip)

  -- | Four places
  data Guess = Guess [Int] deriving Show

    -- | Response
  data Response = TryAgain Int Int -- ^ TryAgain bulls cows
                | Victory 
                deriving (Eq, Show)

  score :: [Int] -> [Int] -> Response
  score a b = case (bulls, cows) of
      (4, 0) -> Victory
      (b, c) -> TryAgain bulls cows
    where
      cows = (length $ intersect a b) - bulls
      bulls = length . filter (\(x,y) -> x == y) $ zip a b