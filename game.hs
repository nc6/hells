module Main where

  import Control.Monad (forever)
  import Data.Char (digitToInt)

  import Hells.Common
  import Hells.Setter

  main :: IO ()
  main = do
    game <- set
    putStrLn $ show game
    forever $ getLine >>= putStrLn . writeAnswer . answer game . readGuess

  readGuess :: String -> Place
  readGuess = Place . map digitToInt

  writeAnswer :: Response -> String
  writeAnswer (TryAgain bulls cows) = (show bulls) ++ " bulls and " ++ (show cows) ++ " cows."
  writeAnswer Victory = "You win!"