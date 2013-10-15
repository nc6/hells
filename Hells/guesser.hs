module Hells.Guesser where
  
  import Hells.Common
  import Hells.Setter
  
  data Move = Move Guess Response

  -- | A state is a series of guesses.
  type State = [Move]

  -- | Guesser takes a state and emits the next guess.
  type Guesser = State -> Guess

  -- | Result - can either win or give up after a certain number of tries.
  data Result = Win State | GiveUp Int

  play :: Guesser -> Game -> Int -> Result
  play guesser game maxTries = move [] where
    move state 
      | length state > maxTries = GiveUp maxTries
      | otherwise = let guess = guesser state in
          case answer game guess of
            Victory -> Win $ (Move guess Victory) : state
            t -> move $ (Move guess t) : state