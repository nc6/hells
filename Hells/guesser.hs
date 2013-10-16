module Hells.Guesser where
  
  import Data.List (intercalate)

  import Hells.Common
  import Hells.Setter
  
  data Move = Move Guess Response deriving Show

  -- | History is maintained by the play function
  type History = [Move]

  class GameState a where 
    szero :: a
    supdate :: Move -> a -> a

  -- | Guesser takes a history and emits the next guess.
  type Guesser a = a -> Guess

  -- | Result - can either win or give up after a certain number of tries.
  data Result = Win History | GiveUp Int deriving Show

  play :: GameState a => Guesser a -> Game -> Int -> Result
  play guesser game maxTries = go [] szero where
    go history state 
      | length history > maxTries = GiveUp maxTries
      | otherwise = let 
            guess = guesser state 
            response = answer game guess
            nextmove = Move guess response
            newhistory = nextmove : history
          in case response of
            Victory -> Win newhistory
            _ -> go newhistory (supdate nextmove state)

  showResult :: Result -> String
  showResult (Win h) = intercalate "\n" . map showMove $ reverse h where
    showMove (Move guess response) = show guess ++ "\n\t" ++ showResponse response
    showResponse Victory = "Victory!"
    showResponse (TryAgain bulls cows) = 
      "You have " ++ (show cows) ++ " cows and " ++ (show bulls) ++ " bulls."
  showResult (GiveUp x) = "Gave up after " ++ (show x) ++ " tries."