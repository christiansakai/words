module Main where

import System.Random ( newStdGen
                     )
import Lib ( grid
           , languages
           , Game
           , makeGame
           , playGame
           , completed
           , fillInBlanks
           )


main :: IO ()
main = do
  gen <- newStdGen

  let
    gameGrid = fillInBlanks gen grid
    game = makeGame gameGrid languages

  playTurn game


playTurn :: Game -> IO ()
playTurn game
  | completed game = putStrLn "You win, congratulations"
  | otherwise      = do
    putStrLn . show $ game

    putStr "Enter a word> "
    word <- getLine

    let newGame = playGame game word 
    playTurn newGame
