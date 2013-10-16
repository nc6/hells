module Main where

  import Control.Monad (forever)
  import Data.Char (digitToInt)
  import System.Environment (getArgs)

  import Hells.Common
  import Hells.Setter
  import Hells.Guesser
  import qualified Hells.Strategy.Simple as Simple

  main :: IO ()
  main = do
    args <- getArgs
    game <- set
    case args of
      ["simple"] -> putStrLn . showResult $ play Simple.guess game 10
      _ -> forever $ getLine >>= putStrLn . writeAnswer . answer game . readGuess

  readGuess :: String -> Guess
  readGuess = Guess . map digitToInt

  writeAnswer :: Response -> String
  writeAnswer (TryAgain bulls cows) = (show bulls) ++ " bulls and " ++ (show cows) ++ " cows."
  writeAnswer Victory = "You win!"