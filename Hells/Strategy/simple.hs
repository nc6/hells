module Hells.Strategy.Simple where

  import Data.List ((\\), filter)
  import Hells.Common
  import Hells.Guesser

  newtype State = State { unState :: [[Int]] }

  instance GameState State where
    szero = State [[a,b,c,d] | a <- [0 .. 9]
                       , b <- [0 .. 9] \\ [a]
                       , c <- [0 .. 9] \\ [a,b]
                       , d <- [0 .. 9] \\ [a,b,c]]
    supdate (Move (Guess g) r) = State . filter (\g2 -> score g g2 == r) . unState

  guess :: Guesser State
  guess = Guess . head . unState