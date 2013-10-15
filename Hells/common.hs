module Hells.Common where

  -- | Four places
  data Guess = Guess [Int] deriving Show

    -- | Response
  data Response = TryAgain Int Int -- ^ TryAgain bulls cows
                | Victory