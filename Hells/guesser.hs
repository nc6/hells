module Hells.Guesser where
  
  import Hells.Common
  
  -- | A state is a series of guesses.
  type State = [Guess]

  -- | Guesser takes a state and emits the next guess.
  type Guesser = State -> Guess

  -- | Result - can either win or give up after a certain number of tries.
  type Result = Win Guess | GiveUp Int

  play :: Guesser -> Game -> Result
  play guesser game = 