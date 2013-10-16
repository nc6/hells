module Hells.Guesser where
  
  import Hells.Common
  import Hells.Setter
  
  data Move = Move Guess Response deriving Show

  class GameState a where 
    hzero :: a
    hupdate :: Move -> a -> a

  -- | History is maintained by the play function
  type History = [Move]

  -- | Guesser takes a history and emits the next guess.
  type Guesser a = a -> Guess

  -- | Result - can either win or give up after a certain number of tries.
  data Result = Win History | GiveUp Int deriving Show

  play :: GameState a => Guesser a -> Game -> Int -> Result
  play guesser game maxTries = move [] hzero where
    move history state 
      | length history > maxTries = GiveUp maxTries
      | otherwise = let 
            guess = guesser state 
            response = answer game guess
            nextmove = Move guess response
            newhistory = nextmove : history
          in case response of
            Victory -> Win newhistory
            _ -> move newhistory (hupdate nextmove state)