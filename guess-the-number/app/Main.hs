module Main where

import Lib ( checkNumber )
import System.Exit (exitSuccess)

main = do
  askForNumber

askForNumber :: IO ()
askForNumber = do
  putStrLn "Input a target..."
  target <- getLine
  if null target
    then do
      putStrLn "Invalid integer"
      askForNumber
    else do
      let targetNumber = (read target :: Int)
      goGuess targetNumber True

goGuess :: Int -> Bool -> IO ()
goGuess target signal = do
  if not signal
    then exitSuccess
    else do
      putStrLn "Input a guess... "
      number <- getLine
      if null number
        then do
          putStrLn "Invalid integer"
          goGuess target True
        else do
          let theNumber = (read number :: Int)
          putStrLn $ checkNumber theNumber target
          goGuess target (theNumber /= target)