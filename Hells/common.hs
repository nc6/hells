module Hells.Common where

  -- | Four places
  data Place = Place [Int] deriving Show
  -- | Guess Place Bulls Cows
  data Guess = Guess Place Int Int deriving Show