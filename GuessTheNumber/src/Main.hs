module GuessTheNumber(main) where

import System.IO
import System.Random
import Utils.IO

main = do
    name <- prompt "Hello! What's your name?\n>>> "
    putStrLn ("Hello, " ++ name ++ "!\n")

    stdGen <- getStdGen
    let (randomNumber, _) = randomR (minRandomNumber, maxRandomNumber) stdGen :: (Int, StdGen)
    putStrLn ("I have random number from " ++ show minRandomNumber ++ " to " ++ show maxRandomNumber ++ ".")
    putStrLn "Can you guess it?\n"

    guessTheNumber randomNumber name attemptCount

minRandomNumber = 1
maxRandomNumber = 10
attemptCount = 3

guessTheNumber :: Int -> String -> Int -> IO ()
guessTheNumber randomNumber name attemptsLeft = do
    if attemptsLeft == 1 then
        putStrLn "You have 1 attempt left."
    else
        putStrLn ("You have " ++ show attemptsLeft ++ " attempts left.")

    userInput <- prompt "What's the number?\n>>> "
    let userNumber = read userInput :: Int

    if userNumber == randomNumber then
        putStrLn ("Congratulations, " ++ name ++ "! My number is " ++ show randomNumber ++ ".")
    else if attemptsLeft > 1 then do
        putStrLn ("Nope, " ++ name ++ ". My number isn't " ++ userInput ++ ".\n")
        guessTheNumber randomNumber name (attemptsLeft - 1)
    else
        putStrLn ("Nope, " ++ name ++ ". My number is " ++ show randomNumber ++ ".\n")
